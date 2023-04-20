"""
This is a module with utility functions for transforming input data into 'F-matrix' style data.

"""
from typing import List, Optional, Tuple, Callable
import numpy as np
import pandas as pd


def create_label_conf(
    df: pd.DataFrame, 
    visit_id: int,
    label_column: str,
    *args: Tuple[int, float], 
    core: Optional[Tuple[int, int]] = (7, 3)
) -> pd.DataFrame:
    """Create a mapping between days and (label, confidence) through step function.

    Parameters
    ----------
    df : pd.DataFrame of shape (number of visits, :)
        The dataset with information about visits dates and their outcomes.

    visit_id : int
        The unique indetifier of a visit.

    label_column: str
        Name of the column containing label, information about the chosen 
        psychiatric assessment.

    *args: 
        a list of >= 1 tuples Tuple[inf, float] that define confidence factor
        about the label extrapolation relative to the `core` argument.
        For example, *args= (14, .5), (7, .75) would result in assigning all
        data down to 7 days before the core period with confidence = 0.75, 
        and all days from day visit_date - 8 (inclusive) to day visit_date - 14 
        with confidence = 0.5.

    core: Tuple[int, int]
        The core ground-truth period, i.e. range of days that should be assigned
        the label with maximum confidence factor equal to 1.
        For example, core=(7, 2) assigns all days down to visit_date - 7, 
        up to visit_date + 2.

    Returns
    -------
    df : pd.DataFrame of shape (number of dates with assigned label, :)
        DataFrame with columns:
        - "date": date that is assigned a label extrapolated from corresponding visit,
        - "conf": a confidence factor about the label dependening on time;
                  "conf" in (0.0, 1.0],
        - "label": the extrapolated label,
        - "visit_id": identifier of the visit on which extrapolation is based.


    Notes
    -----
    Extrapolates label from the date of psychiatric assessment onto surrounding days,
    allowing for custom confidence adjustments based on the distance from the visit.
    For more details, see "Kmita et al., WCCI2022".

    Examples
    --------

    >>> from utils import create_label_conf
    >>> import numpy as np
    >>> import pandas as pd
    >>> df = pd.DataFrame({
    ...     "cgibd_mindState": ["depression"], 
    ...     "visit_date": [pd.Timestamp("2020-02-02")],
    ...     "visit_id": [0]
    ... })
    >>> df_conf = create_label_conf(
    ...     df, 0, "cgibd_mindState",
            (7, .2), (5, .5),
    ...     core=(2, 1)
    ... )
    >>> df_conf
    date  conf       label  visit_id
    0  2020-01-24   0.2  depression         0
    1  2020-01-25   0.2  depression         0
    2  2020-01-26   0.5  depression         0
    3  2020-01-27   0.5  depression         0
    4  2020-01-28   0.5  depression         0
    5  2020-01-29   0.5  depression         0
    6  2020-01-30   0.5  depression         0
    7  2020-01-31   1.0  depression         0
    8  2020-02-01   1.0  depression         0
    9  2020-02-02   1.0  depression         0
    10 2020-02-03   1.0  depression         0
    """

    label = df[label_column].iloc[visit_id]
    visit_date = df.visit_date.iloc[visit_id]

    dates = [
        np.arange(
            visit_date - np.timedelta64(core[0], "D"),
            visit_date + np.timedelta64(core[1] + 1, "D"),
            dtype="datetime64[D]"
        )
    ]
    conf = [np.full(shape=len(dates[0]), fill_value=1., dtype=float)]

    for arg in args:
        new_dates = np.arange(
                visit_date - np.timedelta64(arg[0] + core[0], "D"),
                visit_date - np.timedelta64(core[0], "D"),
                dtype="datetime64[D]"                
            )
        dates.append(new_dates)
        conf.append(np.full(shape=len(new_dates), fill_value=arg[1], dtype=float))
        
    output = pd.DataFrame(
        {"date": np.hstack(dates), "conf": np.hstack(conf)}
    ).groupby("date")["conf"].max().reset_index()
    
    output["label"] = label
    output["visit_id"] = visit_id
    
    return output.sort_values(by="date")

def create_label_conf_func(
    df: pd.DataFrame, 
    visit_id: int, 
    days: int, 
    func: Callable[..., float], 
    core: Optional[Tuple[int, int]] = (7, 3)
) -> pd.DataFrame:
    # TODO: this needs refinement, it's supposed to be a version of `create_label_conf`
    #       where user can supply any function weighting the distance from visit_date
    #       instead of the default "step" function.
    """Create a mapping between days and (label, confidence) through user defined function.

    Parameters
    ----------
    df : pd.DataFrame of shape (number of visits, :)
        The dataset with information about visits dates and their outcomes.

    visit_id : int
        The unique indetifier of a visit.

    days: int
        Defines the beginning of the interval (-days, 0) for which the confidence
        will be extracted according to `func`.

    func: Callable
        A custom function to weight confidence. Note it should obtain values from [0, 1]
        and be supported as a callable. See the example for using gaussian noise with
        custom `sigma` value.

    core: Tuple[int, int]
        The core ground-truth period, i.e. range of days that should be assigned
        the label with maximum confidence factor equal to 1.
        For example, core=(7, 2) assigns all days down to visit_date - 7, 
        up to visit_date + 2.

    Returns
    -------
    df : pd.DataFrame of shape (number of dates with assigned label, :)
        DataFrame with columns:
        - "date": date that is assigned a label extrapolated from corresponding visit,
        - "conf": a confidence factor about the label dependening on time;
                  "conf" in (0.0, 1.0],
        - "label": the extrapolated label,
        - "visit_id": identifier of the visit on which extrapolation is based.

    Notes
    -----
    Extrapolates label from the date of psychiatric assessment onto surrounding days,
    allowing for custom confidence adjustments based on the distance from the visit.
    For more details, see "Kmita et al., WCCI2022".

    Examples
    --------

    >>> from utils import create_label_conf_func
    >>> from functools import partial
    >>> import numpy as np
    >>> import pandas as pd
    >>> df = pd.DataFrame({
    ...     "cgibd_mindState": ["depression"], 
    ...     "visit_date": [pd.Timestamp("2020-02-02")],
    ...     "visit_id": [0]
    ... })
    >>> def gaussian_weight(distance, sigma=1.0):
    ...     return np.exp(-distance**2/(2*sigma**2))
    >>> df_conf = create_label_conf_func(
    ...     df, 0, 5,
    ...     partial(gaussian, sigma=5.),
    ...     core=(2, 2)
    ... )
	>>> df_conf
			date      conf       label  visit_id
	0 2020-01-26  0.486752  depression         0
	1 2020-01-27  0.606531  depression         0
	2 2020-01-28  0.726149  depression         0
	3 2020-01-29  0.835270  depression         0
	4 2020-01-30  0.923116  depression         0
	5 2020-01-31  1.000000  depression         0
	6 2020-02-01  1.000000  depression         0
	7 2020-02-02  1.000000  depression         0
	8 2020-02-03  1.000000  depression         0
	9 2020-02-04  1.000000  depression         0
    """

    label = df.cgibd_mindState.iloc[visit_id]
    visit_date = df.visit_date.iloc[visit_id]

    dates = [
        np.arange(
            visit_date - np.timedelta64(core[0], "D"),
            visit_date + np.timedelta64(core[1] + 1, "D"),
            dtype="datetime64[D]"
        )
    ]
    conf = [np.full(shape=len(dates[0]), fill_value=1., dtype=float)]

    new_dates = np.arange(
            visit_date - np.timedelta64(days + core[0], "D"),
            visit_date - np.timedelta64(core[0], "D"),
            dtype="datetime64[D]"                
        )
    dates.append(new_dates)
    
    conf.append(
        np.array([func(day+1) for day in range(days, 0, -1)], dtype=float)
    )
           
    output = pd.DataFrame({
        "date": np.concatenate(dates), 
        "conf": np.concatenate(conf),
        "label": label,
        "visit_id": visit_id
    }).sort_values(by="date")
        
    return output.reset_index(drop=True)    
    

def create_fmatrix_with_conf(df: pd.DataFrame, label_column: str, labels: List[str]) -> pd.DataFrame:
    """Init n_clusters seeds according to k-means++.

    https://github.com/scikit-learn/scikit-learn/blob/main/sklearn/cluster/_kmeans.py #50

    Parameters
    ----------
    df : pd.DataFrame
        DataFrame with the data to be reshaped.

    label_column : str
        Name of the column containing labels.

    labels : array-like of shape (number of labels,)
        Array containing all possible labels in a desired order.
        Note that it should containg all possible labels, even if they're unseen 
        in a provided `df` dataframe, preserving their format i.e.
        character (preferably) or numeric.

    Returns
    -------
    df: pd.DataFrame
        Dataframe with all columns preserved apart from `label_column`
        that gets dummy-encoded in (number of labels) separate columns.

    Notes
    -----
    No notes

    Examples
    --------

    >>> from utils import create_fmatrix_with_conf
    >>> import numpy as np
    >>> import pandas as pd
    >>> df = pd.DataFrame({
    ... 	'date': [pd.Timestamp('2020-01-26'), pd.Timestamp('2020-02-27'),
    ...             pd.Timestamp('2020-03-01')],
    ... 	'conf': [0.2, 0.2, 1.0],
    ... 	'label': ['depression', 'euthymia', 'euthymia'],
    ... 	'visit_id': [0, 1, 2]
    ... })
    >>> F = create_fmatrix_with_conf(
    ...     df=df, label_column="label", 
    ...     labels=["depression", "euthymia", "dysfunction"])
	>>> F
            date  conf  visit_id  depression  euthymia  dysfunction
    0 2020-01-26   0.2         0           1         0            0
    1 2020-02-27   0.2         1           0         1            0
    2 2020-03-01   1.0         2           0         1            0
    """
    dummied = pd.get_dummies(pd.Categorical(df.loc[:, label_column], categories=labels))
    output = pd.concat([df.drop(columns=label_column), dummied], axis=1)

    return output



