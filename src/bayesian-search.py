import sys
import subprocess
import os
import multiprocessing as mp
import pandas as pd
from sklearn.metrics import mean_squared_error
from sklearn.metrics import r2_score
import numpy as np
import json
import pprint
import time
from bayes_opt import BayesianOptimization
from bayes_opt.logger import JSONLogger
from bayes_opt.event import Events


def get_repo_root():
    """Get the root directory of the repo."""
    dir_in_repo = os.path.dirname(os.path.abspath('__file__'))
    return subprocess.check_output('git rev-parse --show-toplevel'.split(),
                                   cwd=dir_in_repo,
                                   universal_newlines=True).rstrip()


ROOT_dir = get_repo_root()
sys.path.append(ROOT_dir)


class SensorCalibrationSegment:
    def __init__(self, data=None, here_seg=None, direction=None):
        self.data = data
        self.here_seg = here_seg
        self.direction = direction

    def flow_cal(self, Capacity=None, u_f=None, u=None, alpha=None, beta=None):
        if u_f > u:
            return Capacity*((u_f/u - 1) / alpha)**(1/beta)
        else:
            return 0

    def spd2flow(self, alpha=None, beta=None):
        self.data.loc[:, 'flow_fit'] = self.data.apply(lambda row: self.flow_cal(Capacity=row['capacity'],
                                                                             u_f=row['speed_ff'],
                                                                             u=row['speed'],
                                                                             alpha=alpha, beta=beta), axis=1)
        return -mean_squared_error(self.data.loc[:, 'flow_fit'], self.data.loc[:, 'flow'])

    def bo_search(self):
        # Start timing the code
        start_time = time.time()

        # Bounded region of parameter space
        pbounds = {'alpha': (0.01, 0.99), 'beta': (1, 10)}

        optimizer = BayesianOptimization(
            f=self.spd2flow,
            pbounds=pbounds,
            random_state=98,
        )

        logger = JSONLogger(path=ROOT_dir + "/results/logs_" + self.here_seg + "_" + str(self.direction) + ".json")
        optimizer.subscribe(Events.OPTIMIZATION_STEP, logger)
        optimizer.maximize(
            init_points=8,
            n_iter=100,
        )
        print(optimizer.max)
        print(self.here_seg, "is done. Elapsed time was %g seconds" % (time.time() - start_time))
        return {'alpha': optimizer.max['params']['alpha'],
                'beta': optimizer.max['params']['beta'],
                'mse': -optimizer.max['target']}


def search_func2parallel(data=None, here_seg=None, direction=None):
    """
    :type here_seg: a string of the here segment
    """
    g = SensorCalibrationSegment(data=data, here_seg=here_seg, direction=direction)
    opti = g.bo_search()
    opti['here_seg'] = here_seg
    opti['direction'] = direction
    return opti


if __name__ == '__main__':
    df = pd.read_csv(ROOT_dir + '/dbs/flow3m.csv')
    df = df.loc[(df.speed != 0) & (df.speed_ff > df.speed), :]
    r = list(df.groupby(['HERE_segID', 'direction']))
    pool = mp.Pool(mp.cpu_count())
    results = pool.starmap(search_func2parallel, [(x[1], x[0][0], x[0][1]) for x in r])
    df_res = pd.DataFrame(results)
    df_res.to_csv(ROOT_dir + '/results/params.csv', index=False)
    pool.close()


