import matplotlib.dates as mdates
import matplotlib as mpl

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

def timeseries_formatting( ax, spines=["bottom"], which="y", title=None, ylabel=None, xlabel=None, xsize=12, ysize=12,
                           xlims=None, ylims=None ):
    # Properly label timeseries
    ax.xaxis.set_minor_locator( mdates.MonthLocator() )
    ax.xaxis.set_minor_formatter( mdates.DateFormatter( '%b' ) )
    ax.xaxis.set_major_locator( mdates.YearLocator() )
    ax.xaxis.set_major_formatter( mdates.DateFormatter( '%Y %b' ) )

    # Remove spines
    [ax.spines[j].set_visible( False ) for j in ax.spines if j not in spines]

    # Format axes ticks
    ax.tick_params( axis="x", bottom=False, which="both", rotation=90, labelbottom=True, labelsize=xsize )
    ax.tick_params( axis="y", left=False, which="both", labelleft=True, labelsize=ysize )

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