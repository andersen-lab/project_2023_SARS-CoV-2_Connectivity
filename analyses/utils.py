import matplotlib.dates as mdates
import matplotlib as mpl
from datetime import datetime as dt
from datetime import timedelta
import time

def get_okabe_ito_palette():
    return ["#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"]

def setup_plotting_standards():
    prop = mpl.font_manager.FontProperties('Roboto')
    mpl.rcParams['font.sans-serif'] = prop.get_name()
    mpl.rcParams['font.family'] = 'sans-serif'
    mpl.rcParams['font.weight']=300
    mpl.rcParams['axes.labelweight']=300
    mpl.rcParams['font.size']=16

    mpl.rcParams['figure.dpi'] = 200

    COLOR = '#343434'
    mpl.rcParams['text.color'] = COLOR
    mpl.rcParams['axes.labelcolor'] = COLOR
    mpl.rcParams['xtick.color'] = COLOR
    mpl.rcParams['ytick.color'] = COLOR

def timeseries_formatting( ax ):
    """
    Specifies the proper date formatting of the xaxis.
    """
    # Properly label timeseries
    ax.xaxis.set_minor_locator( mdates.MonthLocator() )
    ax.xaxis.set_minor_formatter( mdates.DateFormatter( '%b' ) )
    ax.xaxis.set_major_locator( mdates.YearLocator() )
    ax.xaxis.set_major_formatter( mdates.DateFormatter( '%Y %b' ) )
        
def basic_formatting( ax, spines=["bottom"], which="y", title=None, ylabel=None, xlabel=None, xsize=12, ysize=12, xlims=None, ylims=None ):
    """
    Specifies the basic formatting of all plots.
    """
    # Remove spines
    [ax.spines[j].set_visible( False ) for j in ax.spines if j not in spines]

    # Format axes ticks
    ax.tick_params( axis="x", bottom=False, which="both", rotation=90, labelbottom=True, labelsize=xsize, pad=0 )
    ax.tick_params( axis="y", left=False, which="both", labelleft=True, labelsize=ysize, pad=0 )

    # Label axes
    ax.set_xlabel( xlabel, fontsize=xsize )
    ax.set_ylabel( ylabel, fontsize=ysize )
    ax.set_title( title )

    # Add a simple grid
    ax.grid( which="both", axis=which, linewidth=1, color="#F1F1F1", zorder=1 )

    # Add the xlims
    if xlims:
        ax.set_xlim( xlims )
    if ylims:
        ax.set_ylim( ylims )

def _toYearFraction( date, date_format="%Y-%m-%d" ):
    """ Converts datetime object to a decimal year
    Parameters
    ----------
    date: datetime.datetime
        date to be converted.

    Returns
    -------
    float
        date in decimal year format.
    """
    def sinceEpoch( d ): # returns seconds since epoch
        return time.mktime( d.timetuple() )

    date = dt.strptime( date, date_format )
    
    year = date.year
    start_of_this_year = dt( year=year, month=1, day=1 )
    start_of_next_year = dt( year=year+1, month=1, day=1 )

    year_elapsed = sinceEpoch( date ) - sinceEpoch( start_of_this_year )
    year_duration = sinceEpoch( start_of_next_year ) - sinceEpoch( start_of_this_year )
    fraction = year_elapsed / year_duration

    return date.year + fraction
        
def dec_to_date( date_str ):
    """
    Converts a decimal date into a datetime object. 
    Useful for interacting with output from BEAST.
    """
    if date_str is None:
        return None
    year = int( date_str )
    rem = date_str - year
    base = dt( year, 1, 1 )
    result = base + timedelta( seconds=(base.replace( year=base.year + 1 ) - base).total_seconds() * rem )
    return result

def hpd(data, level):
    """
    Return highest posterior density interval from a list,
    given the percent posterior density interval required.
    """
    d = list(data)
    d.sort()

    nData = len(data)
    nIn = int(round(level * nData))
    if nIn < 2 :
        return None
    #raise RuntimeError("Not enough data. N data: %s"%(len(data)))
 
    i = 0
    try:
        r = d[i+nIn-1] - d[i]
    except IndexError:
        print( i )
        print( nIn )
        print( d )
        raise

    for k in range(len(d) - (nIn - 1)) :
        rk = d[k+nIn-1] - d[k]
        if rk < r :
            r = rk
            i = k

    assert 0 <= i <= i+nIn-1 < len(d)
 
    return (d[i], d[i+nIn-1])

