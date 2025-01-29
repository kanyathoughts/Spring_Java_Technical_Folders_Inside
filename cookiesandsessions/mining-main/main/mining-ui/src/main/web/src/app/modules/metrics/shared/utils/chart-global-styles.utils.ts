import { BarOptions, ColorAttr, ColumnOptions, Datum, G2, LineOptions, PieOptions } from '@antv/g2plot';
import { Axis } from '@antv/g2plot/lib/types/axis';
import { Legend } from '@antv/g2plot/lib/types/legend';
import { ChartDataInterface } from '../components/metrics-card/metrics-card.interface';
import { NumberFormatter } from '@app/shared/pipes/number-formatter';
import { Injectable } from '@angular/core';
import { Label } from '@antv/g2plot/lib/types/label';
import { TaxonomyPojo } from '@innowake/mining-api-angular-client';

const defaultLabelStyle: { 'fontFamily': string; 'fontSize': number } = {
  fontFamily: 'Open Sans',
  fontSize: 14
};

const interactionType = {
  hover: 'hover-cursor',
  active: 'element-active'
};

const padding = 'auto';
const appendPadding = [28, 8, 32, 0];
const syncViewPadding = true;
const autoFit = true;

const xAxis: Axis = {
  label: {
    autoHide: false,
    autoRotate: true,
    autoEllipsis: true
  },
  verticalLimitLength: 65
};

const animationDuration = 300;

const legend: Legend = {
  layout: 'horizontal',
  position: 'top',
  flipPage: false
};

const configLabel: { [name: string]: Label } = {
  'defaultLabel': {
    style: defaultLabelStyle,
    position: 'top',
    offsetY: 8,
  },
  'stackColumnLabel': {
    style: defaultLabelStyle,
    position: 'middle',
    layout: [
      { type: 'interval-adjust-position' },
      { type: 'interval-hide-overlap' },
      { type: 'adjust-color' },
    ],
  },
  'stackedBarLabel': {
    style: {
      fontFamily: 'Open Sans'
    },
    position: 'left',
    offset: 4,
    layout: [
      { type: 'adjust-color' }
    ]
  }
};

// eslint-disable-next-line @typescript-eslint/ban-types
const themes: { [name: string]: object } = {
  'classTheme': {
    'paletteQualitative10':
      ['#5B8FF9', '#61DDAA', '#65789B', '#F6BD16', '#7262fd', '#78D3F8', '#9661BC', '#F6903D', '#008685', '#F08BB4'],
    'paletteQualitative20':
      ['#5B8FF9', '#CDDDFD', '#61DDAA', '#CDF3E4', '#65789B', '#CED4DE', '#F6BD16', '#FCEBB9', '#7262fd', '#D3CEFD',
        '#78D3F8', '#D3EEF9', '#9661BC', '#DECFEA', '#F6903D', '#FFE0C7', '#008685', '#BBDEDE', '#F08BB4', '#FFE0ED']
  },
  'oppoTheme': {
    'paletteQualitative10':
      ['#661900', '#E6450F', '#FF8C00', '#FFCB33', '#E0F2EB', '#1AC5FF', '#007FFF', '#0040FF', '#001F7F'],
    'paletteQualitative20':
      ['#661900', '#B22C00', '#E6450F', '#FF6500', '#FF8C00', '#FFB200', '#FFCB33', '#FFDF80', '#E0F2EB',
        '#66D8FF', '#1AC5FF', '#00A5FF', '#007FFF', '#0059FF', '#0040FF', '#002CB2', '#001F7F']
  },
  'seqTheme': {
    'paletteQualitative10':
      ['#096dd9', '#207ae5', '#3887ed', '#4e94f3', '#63a1f8', '#76affb', '#88bdfd', '#99cbff', '#aad9ff', '#bae7ff'],
    'paletteQualitative20':
      ['#096dd9', '#1273df', '#1e79e4', '#2a7fe9', '#3686ec', '#418cf0', '#4b92f2', '#5598f5', '#5f9ff7', '#68a5f9',
        '#71abfa', '#7ab2fc', '#82b8fd', '#8bbffd', '#93c5fe', '#9bccff', '#a3d3ff', '#abd9ff', '#b2e0ff', '#bae7ff']
  },
  'semaTheme': {
    'Low': '#A9DACC', 'Medium': '#42b3d5', 'High': '#3073AE', 'Very High': '#171E6D'
  },
  'singleTheme': {
    'color': '#096dd9'
  },
  'stackColumnTheme': {
    'paletteQualitative10':
      ['#faad14', '#096dd9']
  }
};

/**
 * Global styles for charts
 */
@Injectable({
  providedIn: 'root'
})
export class ChartGlobalStyles {
  constructor(private numberFormatter: NumberFormatter) {
    G2.registerInteraction(interactionType.hover, {
      showEnable: [
        { trigger: 'plot:mouseenter', action: 'cursor:pointer' },
        { trigger: 'legend:mouseleave', action: 'cursor:pointer' }
      ],
    });
    G2.registerInteraction('element-link', {
      start: [{ trigger: 'interval:mouseenter', action: 'element-link-by-color:link' }],
      end: [{ trigger: 'interval:mouseleave', action: 'element-link-by-color:unlink' }],
    });
  }

  /**
   * Gets default style configuration for Column Charts
   * @param data Data displayed in the chart
   * @param xField The data field name for the x axis
   * @param yField The data field name for the y axis
   * @param themeName Name of the theme to apply
   * @param isStack boolean to set stack type for chart
   * @param seriesField The data field name for categorization for stack charts
   * @param labelOption to set the labels
   * @param showPointer to show pointer cursor on chart
   * @param showElementLink to show the element lint on chart
   * @returns the configuration for Column chart
   */
  public getColumnConfig(data: Array<{ key: string, value: number }>, xField: string, yField: string, themeName: string = 'singleTheme',
    isStack: boolean = false, seriesField?: string, labelOption = 'defaultLabel', showPointer: boolean= false, showElementLink: boolean= false): ColumnOptions {
    const color: ColorAttr = this.getColorScheme(xField, themeName, isStack);
    const label = configLabel[labelOption];
    const labelStyle: Label = {...label};
    let interactions: Array<{ type: string }> = [{ type: interactionType.active }];
    if (showElementLink) {
      interactions = [{ type: 'element-link' },{ type: 'element-highlight-by-color' }];
    }
    if (showPointer) {
      interactions.push({ type: interactionType.hover });
    }
    labelStyle['fill'] = '#fff';
    return {
      data,
      meta: {
        value: {
          formatter: (value: number) => this.numberFormatter.transform(value)
        }
      },
      isStack,
      xField,
      yField,
      seriesField,
      label: labelStyle,
      padding,
      appendPadding,
      syncViewPadding,
      autoFit,
      xAxis,
      color,
      theme: {
        styleSheet: themes[themeName]
      },
      animation: {
        appear: {
          duration: animationDuration,
        },
      },
      legend,
      interactions};
  }

  /**
   * Gets default style configuration for Bar Charts
   * @param data Data displayed in the chart
   * @param xField The data field name for the x axis
   * @param yField The data field name for the y axis
   * @param themeName Name of the theme to apply
   * @param showPointer to show pointer cursor on chart
   * @returns the configuration for Bar chart
   */
  public getBarConfig(data: TaxonomyPojo[] | Array<{ key: string, value: number }>,
    xField: string, yField: string, themeName: string = 'singleTheme', showPointer: boolean = false): BarOptions {
    const labelStyle = Object.assign({}, defaultLabelStyle);
    labelStyle['rotate'] = false;
    const color: ColorAttr = this.getColorScheme(yField, themeName, false);
    return {
      data,
      meta: {
        value: {
          formatter: (value: number) => this.numberFormatter.transform(value)
        }
      },
      xField,
      yField,
      label: {
        style: labelStyle,
        autoRotate: true,
      },
      padding,
      appendPadding,
      syncViewPadding,
      autoFit,
      color,
      theme: {
        styleSheet: themes[themeName]
      },
      animation: {
        appear: {
          duration: animationDuration,
        },
      },
      legend,
      interactions: showPointer ? [{ type: interactionType.active }, { type: interactionType.hover }] : [{ type: interactionType.active }]
    };
  }

    /**
     * Gets default style configuration for Grouped Bar Charts
     * @param data Data displayed in the chart
     * @param xField The data field name for the x axis
     * @param yField The data field name for the y axis
     * @param themeName Name of the theme to apply
     * @param showPointer to show pointer cursor on chart
     * @returns the configuration for Grouped Bar chart
     */
    public getGroupedBarConfig(data: Array<{label: string, name: string, value: number}>,
      xField: string, yField: string, seriesField?: string, themeName: string = 'classTheme',showPointer: boolean = false): BarOptions {
      const label = configLabel['stackedBarLabel'];
      return {
        data,
        meta: {
          value: {
            formatter: (value: number) => this.numberFormatter.transform(value)
          }
        },
        isGroup: true,
        xField,
        yField,
        seriesField,
        marginRatio: 0,
        label,
        padding,
        appendPadding,
        syncViewPadding,
        autoFit,
        color: ['#61DDAA', '#5B8FF9'],
        theme: {
          styleSheet: themes[themeName]
        },
        animation: {
          appear: {
            duration: animationDuration,
          },
        },
        legend,
        interactions: showPointer ? [{ type: interactionType.active }, { type: interactionType.hover }] : [{ type: interactionType.active }]
      };
    }

  /**
   * Gets default style configuration for Pie Charts
   * @param data Data displayed in the chart
   * @param angleField The data field name corresponding to the sector slice size (radians)
   * @param colorField The data field name corresponding to the sector color map
   * @param themeName Name of the theme to apply
   * @param showPointer to show pointer cursor on chart
   * @returns the configuration for Pie chart
   */
  public getPieConfig(data: ChartDataInterface[] | TaxonomyPojo[], angleField: string, colorField: string, themeName: string = 'singleTheme',
    showPointer: boolean = false): PieOptions {
    const labelStyle = Object.assign({}, defaultLabelStyle);
    labelStyle['rotate'] = false;
    labelStyle['labelLine'] = 0;
    labelStyle['autoHide'] = true;
    const color: ColorAttr = this.getColorScheme(colorField, themeName);
    return {
      data,
      meta: {
        value: {
          formatter: (value: number) => this.numberFormatter.transform(value)
        }
      },
      angleField,
      colorField,
      radius: 0.8,
      label: {
        style: labelStyle,
        type: 'outer',
        autoRotate: false,
        offset: 16
      },
      padding,
      appendPadding,
      syncViewPadding,
      autoFit,
      color,
      theme: {
        styleSheet: themes[themeName]
      },
      animation: {
        appear: {
          animation: 'grow-in-xy',
          duration: animationDuration,
        },
      },
      legend,
      interactions: showPointer ? [{ type: interactionType.active }, { type: interactionType.hover }] : [{ type: interactionType.active }]
    };
  }
  /**
   * Gets default style configuration for Line Charts
   * @param data Data displayed in the chart
   * @param xField The data field name for the x axis
   * @param yField The data field name for the y axis
   * @param themeName Name of the theme to apply
   * @param seriesField The data field name for categorization for multi-line charts
   * @param showPointer to show pointer cursor on chart
   * @returns the configuration for Line chart
   */
  public getLineConfig(data: ChartDataInterface[],
    xField: string, yField: string, themeName: string = 'singleTheme', seriesField?: string, showPointer?: boolean): LineOptions {
    const color: ColorAttr = this.getColorScheme(seriesField ? seriesField : xField, themeName);
    return {
      data,
      meta: {
        value: {
          formatter: (value: number) => this.numberFormatter.transform(value)
        }
      },
      xField,
      yField,
      seriesField,
      padding,
      appendPadding,
      syncViewPadding,
      autoFit,
      xAxis: {
        label: null
      },
      color,
      theme: {
        styleSheet: themes[themeName]
      },
      animation: {
        appear: {
          animation: 'wave-in',
          duration: animationDuration,
        },
      },
      legend,
      interactions: showPointer ? [{ type: interactionType.active }, { type: interactionType.hover }] : [{ type: interactionType.active }]
    };
  }


  /**
   * Gets default style configuration for Donut Charts
   * @param  data Data displayed in the chart
   * @param angleField The data field name corresponding to the sector slice size (radians)
   * @param colorField The data field name corresponding to the sector color map
   * @param themeName Name of the theme to apply
   */
  public getDonutConfig(data: ChartDataInterface[] | TaxonomyPojo[], angleField: string, colorField: string, themeName: string = 'singleTheme',
    showPointer?: boolean): PieOptions {
      const labelStyle = Object.assign({}, defaultLabelStyle);
      labelStyle['rotate'] = false;
      labelStyle['labelLine'] = 0;
      labelStyle['autoHide'] = true;
      const color: any = (value: any) => {
      const colorDecider = value.key.split(':');
      if (colorDecider[2] === '-1') {
        return '#bfbfbf';
      } else {
        return themes[themeName]['paletteQualitative10'][data.findIndex((dataItem: any) => dataItem.index === +value.key.split(':')[2])];
      }
    };
    return {
      data,
      meta: {
        value: {
          formatter: (value: number) => this.numberFormatter.transform(value)
        }
      },
      tooltip: {
        formatter: (datum: Datum) => this.toolTipCreator(datum),
        // Template for custom tooltip
        itemTpl: `
        <li data-index style="margin: 0px; list-style-type: none; padding: 0px; min-width: 250px; max-width: 320px">
          <span style="background: {color}; width: 8px; height: 8px; border-radius: 50%; display: inline-block; margin-right: 8px;"></span>
          <span>{name}</span>: 
          <span style="display: inline-block; float: right">{value}</span>
          <p style="margin-top: 10px">{description}</p>
        </li>
      `
      },
      angleField,
      colorField,
      radius: 0.8,
      innerRadius: 0.6,
      label: {
        style: labelStyle,
        type: 'inner',
        autoRotate: false,
        offset: '-50%',
        content: ''
      },
      state: {
        active: {
          style: {
            fillOpacity: 0.65,
            lineWidth: 1,
          },
        },
        inactive: {
          style: {
          fillOpacity: 0.40,
          lineWidth: 2,
          },
        }
      },
      padding,
      syncViewPadding,
      autoFit,
      color,
      theme: {
        styleSheet: themes[themeName]
      },
      animation: {
        appear: {
          animation: 'grow-in-xy',
          duration: animationDuration,
        },
      },
      legend: false,
      interactions: showPointer ? [{ type: interactionType.active }, { type: interactionType.hover }] : [{ type: interactionType.active }],
      statistic: {
        title: false,
        content: {
          style: {
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            whiteSpace: 'wrap',
            width: '80px',
            fontSize: '14px',
            fontWeight: 'normal',
            lineHeight: '22px'
          }
        },
      },
    };
  }

  private toolTipCreator(datum: Datum) {
    const toolTipArray = datum.key.split(':');
    const ToolTip: {name: string; value: string; description?: string}
     = {name: toolTipArray[0], value: toolTipArray[1]};
    if (toolTipArray.length === 4 && toolTipArray[2] !== 'null') {
      return { ...ToolTip, description: toolTipArray[2]};
    }
    return ToolTip;
  }

  private getColorScheme(field: string, themeName: string, isStack: boolean = true): ColorAttr {
    let color: ColorAttr = field;
    if (themeName === 'semaTheme') {
      color = (data) => {
        const key = data[field];
        return themes.semaTheme[key];
      };
    } else if (isStack) {
      color = '';
    }
    return color;
  }
}
