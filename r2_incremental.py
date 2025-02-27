import numpy as np
from sortedcontainers import SortedList

def dominates(p, q):
    """
    Returns whether p dominates q.
    """
    return p[0] <= q[0] and p[1] <= q[1] and p != q

def exclusive_hv(sl, idx, ref):
    """
    Computes the exclusive hypervolume of sl[idx] w.r.t. sl and reference point ref.
    """
    if not dominates(sl[idx], ref):
        return 0

    if idx == 0:
        left_neighbor_f2 = ref[1]
    else:
        left_neighbor_f2 = sl[idx - 1][1]

    if idx == len(sl) - 1:
        right_neighbor_f1 = ref[0]
    else:
        right_neighbor_f1 = sl[idx + 1][0]

    right_neighbor_f1 = min(right_neighbor_f1, ref[0])
    left_neighbor_f2 = min(left_neighbor_f2, ref[1])

    return (left_neighbor_f2 - sl[idx][1]) * (right_neighbor_f1 - sl[idx][0])

def utility(y1, y2, y2p):
    """
    Compute the Tchebycheff utility of the axis-parallel segment u(y1, [y2,y2p]).
    """
    if y1 == np.inf:
        return 0

    if y2p == np.inf:
        wp = 1
    else:
        wp = y2p / (y1 + y2p)

    if y2 == np.inf:
        w = 1
    else:
        w = y2 / (y1 + y2)

    return 0.5 * y1 * (wp**2 - w**2)

def exclusive_r2(sl, idx, ideal):
    """
    Compute the exclusive R2 contribution of sl[idx] w.r.t. sl and ideal point ideal.
    """
    center = (sl[idx][0] - ideal[0], sl[idx][1] - ideal[1])

    if idx == 0:
        left_neighbor = (np.inf, np.inf)
    else:
        left_neighbor = (sl[idx - 1][0] - ideal[0], sl[idx - 1][1] - ideal[1])

    if idx == len(sl) - 1:
        right_neighbor = (np.inf, np.inf)
    else:
        right_neighbor = (sl[idx + 1][0] - ideal[0], sl[idx + 1][1] - ideal[1])

    r2_f1 = utility(center[0], center[1], left_neighbor[1]) - \
            utility(right_neighbor[0], center[1], left_neighbor[1])
    r2_f2 = utility(center[1], center[0], right_neighbor[0]) - \
            utility(left_neighbor[1], center[0], right_neighbor[0])

    return r2_f1 + r2_f2

def compute_indicators_archive(archive, ideal, ref):
    """
    Compute the history of R2 and HV indicator values for evaluation archive
    w.r.t. ideal point ideal and reference point ref.
    """
    sl = SortedList()

    r2 = 0
    r2_history = []

    hv = 0
    hv_history = []

    for p in archive:
        if len(sl) == 0:
            sl.add(p)
            idx = 0
            r2 += exclusive_r2(sl, idx, ideal)
            hv += exclusive_hv(sl, idx, ref)
        else:
            idx = sl.bisect(p)
            left_boundary = (idx == 0)
            idx = max(0, idx - 1)

            if not dominates(sl[idx], p) and sl[idx] != p:
                if not left_boundary:
                    idx += 1

                while idx < len(sl) and dominates(p, sl[idx]):
                    r2 -= exclusive_r2(sl, idx, ideal)
                    hv -= exclusive_hv(sl, idx, ref)
                    sl.remove(sl[idx])

                sl.add(p)

                r2 += exclusive_r2(sl, idx, ideal)
                hv += exclusive_hv(sl, idx, ref)
        # print(f"HV: {hv}")
        # print(f"R2: {r2}")
        r2_history.append(r2)
        hv_history.append(hv)

    return (r2_history, hv_history)
