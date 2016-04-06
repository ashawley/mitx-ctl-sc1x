/**
 * MITx: CTL.SC1x Supply Chain Fundamentals
 *
 * Week 2: Practice Problems
 *
 * https://www.edx.org/course/supply-chain-fundamentals-mitx-ctl-sc1x
 *
 * Educational material reproduced in the source code comments:
 * Copyright (C) 2014-2016, MIT, All Rights Reserved
 *
 * Source code is released under the MIT license:
 * Copyright (C) 2016, Aaron S. Hawley
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package org.edx.mitx.ctl.sc1x

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.stats.MeanAndVariance

import org.specs2.mutable.Specification

class Week2Spec extends Specification {

  /**
   * Practice Problem 1: Shah Alam Palm Oil Company
   * 
   * Palm oil is harvested from the fruit of oil palm trees and is
   * widely used as a cooking oil throughout Africa, Southeast Asia,
   * and parts of Brazil.  It is becoming widely used throughout the
   * world as it is a lower cost alternative to other vegetable oils
   * and has other attractive properties.
   * 
   * You are working for the Shah Alam Palm Oil Company (SAPOC) that
   * harvests, processes, and sells palm oil throughout the region.
   * You are asked to review the sales volume (in pounds) of your
   * premium palm oil by one of your customers, a local grocery store
   * in the region.
    */
  trait PalmOil extends org.specs2.specification.Scope {
    val csv =
      """Month	Year	Demand x(t)
        |Jan	2012	584
        |Feb	2012	552
        |Mar	2012	544
        |Apr	2012	576
        |May	2012	585
        |Jun	2012	784
        |Jul	2012	1026
        |Aug	2012	1098
        |Sep	2012	666
        |Oct	2012	504
        |Nov	2012	576
        |Dec	2012	1224
        |Jan	2013	720
        |Feb	2013	729
        |Mar	2013	747
        |Apr	2013	621
        |May	2013	819
        |Jun	2013	1260
        |Jul	2013	1368
        |Aug	2013	1530
        |Sep	2013	873
        |Oct	2013	976
        |Nov	2013	624
        |Dec	2013	1416
        |Jan	2014	855
        |Feb	2014	528
        |Mar	2014	729
        |Apr	2014	800
        |May	2014	744
        |Jun	2014	928
        |Jul	2014	1560
        |Aug	2014	1746
        |Sep	2014	909
        |Oct	2014	1773
        |Nov	2014	2000
        |Dec	2014	1512"""
    val data: Array[Array[String]] =
      csv.stripMargin.split("\n").map(_.split("\t"))

    val demand = DenseVector(data.drop(1).map(_(2).toDouble))
    val naiveForecast = demand.copy
    val cumulativeForecast =
      DenseVector.tabulate(demand.length) {
        i => breeze.stats.mean(demand(0 to i))
      }
    val horizon = 12
    val horizonForecast = DenseVector.tabulate(demand.length - horizon + 1) {
      i => breeze.stats.mean(demand(i to i + horizon - 1))
    }
  }

  /**
   * Part 3
   * 
   * What is the forecast for demand in January 2015 using a naive
   * model?
   */
  "W2PP1P3Q1" in {
    new PalmOil {
      demand.length === naiveForecast.length

      naiveForecast(-1) === 1512
    }
  }

  /**
   * What is the forecast for demand in January 2015 using a
   * cumulative model?
   */
  "W2PP1P3Q2" in {
    new PalmOil {
      demand.length === cumulativeForecast.length

      cumulativeForecast(-1) must be ~(958.0 +/- 0.5)
    }
  }

  /**
   * What is the forecast for demand in January 2015 using a 12 Period
   * Moving Average model?
   */
  "W2PP1P3Q3" in {
    new PalmOil {
      demand.length === horizonForecast.length + horizon - 1

      horizonForecast(-1) must be ~(1174.0 +/- 0.5)
    }
  }

  /**
   * Part 4
   * 
   * What is the root mean square error (RMSE) for a next period
   * forecast for these three years of demand using a Naive
   * model?
   */
  "W2PP1P4Q1" in {
    new PalmOil {
      demand(2 to -1).length === naiveForecast(1 to -2).length

      val forecastError = demand(1 to -1) - naiveForecast(0 to -2)

      val rmse = breeze.signal.rootMeanSquare(forecastError)
      rmse must beCloseTo(383.73, 5.significantFigures)
    }
  }

  /**
   * What is the root mean square error (RMSE) for a next period
   * forecast for these three years of demand using a using a
   * Cumulative model?
   */
  "W2PP1P4Q2" in {
    new PalmOil {
      demand(1 to -1).length === cumulativeForecast(0 to -2).length

      val forecastError = demand(1 to -1) - cumulativeForecast(0 to -2)

      val rmse = breeze.signal.rootMeanSquare(forecastError)
      rmse must be beCloseTo(419.89, 5.significantFigures)
    }
  }

  /**
   * What is the root mean square error (RMSE) for a next period
   * forecast for these three years of demand using a using a 12
   * Period Moving Average model?
   */
  "W2PP1P4Q3" in {
    new PalmOil {
      demand(horizon to -1).length === horizonForecast(0 to -2).length

      val forecastError = demand(horizon to -1) - horizonForecast(0 to -2)

      val rmse = breeze.signal.rootMeanSquare(forecastError)
      rmse must beCloseTo(423.33, 5.significantFigures)
    }
  }

  /**
   * Practice Problem 2: Forecasting Ordroid Devices
   * 
   * You have just been hired by a company that manufactures mid-range
   * communication devices that use the Ordroid open source operating
   * system.  The company is focused on innovating its products and
   * has not put much thought on its inventory or forecasting
   * capabilities.  Your boss thinks there might be a problem in the
   * forecasting of the Ordroid Devices and wants you to figure it
   * out.  The Ordroid, far from being new to the market, has been out
   * for two years.
   * 
   * Knowing this, you have asked for data on both years of historical
   * sales as well as any forecasts, promotions, pricing changes, or
   * competitive analyses made during this time.  Your boss laughs and
   * provides you with all the data they have: the last six months of
   * sales.  You ask to meet with the current demand planner for the
   * Ordroid Devices and she tells you that they use a forecasting
   * algorithm of her own design and there is no documentation.
   */
  trait Ordroids extends org.specs2.specification.Scope {
    val csv =
      """Month (t)	Actual x(t)	Forecast x^(t)
        |May	1509	1900
        |June	1610	1200
        |July	1708	2100
        |August	1866	1300
        |September	2092	2500
        |October	2390	1500"""
    val data: Array[Array[String]] =
      csv.stripMargin.split("\n").map(_.split("\t"))

    val actual = DenseVector(data.drop(1).map(_(1).toDouble))
    val forecast = DenseVector(data.drop(1).map(_(2).toDouble))
    val deviation = actual - forecast
  }

  /**
   * Part 1A
   * 
   * As a first step, you want to calculate some different performance
   * metrics for the small data sample. Recall that the definition of
   * the error term is the Actual demand minus the Forecasted demand.
   * 
   * What is the mean deviation of the forecasts in this data sample?
   */
  "W2PP2P1A" in {
    new Ordroids {
      breeze.stats.mean(deviation) === 112.5
    }
  }

  /**
   * Part 1B
   * 
   * What is the mean absolute deviation (MAD) of the forecasts in
   * this data sample?
   */
  "W2PP2P1B" in {
    new Ordroids {
      breeze.stats.mean(breeze.numerics.abs(deviation)) === 509.5
    }
  }

  /**
   * Part 1C
   * 
   * What is the root mean square error (RMSE) of the forecasts in
   * this data sample?
   */
  "W2PP2P1C" in {
    new Ordroids {
      breeze.signal.rootMeanSquare(deviation) must be ~(540.6 +/- 0.1)
    }
  }

  /**
   * Part 1D
   * 
   * What is the mean percent error (MPE) of the forecasts in this
   * data sample?
   */
  "W2PP2P1D" in {
    new Ordroids {
      breeze.stats.mean(deviation :/ actual) must be ~(0.041 +/- 0.1)
    }
  }

  /**
   * Part 1E
   * 
   * What is the mean absolute percent error (MAPE) of the forecasts
   * in this data sample?
   */
  "W2PP2P1E" in {
    new Ordroids {
      val absolutePercentageError = breeze.numerics.abs(deviation :/ actual)
      val mape = breeze.stats.mean(absolutePercentageError) 
      mape must be ~(0.269 +/- 0.1)
    }
  }

  /**
   * Practice Problem 4: TrainMax
   *
   * TrainMax is a manufacturer of high-end specialty engine equipment
   * for high speed trains.  They produce parts that are sent to
   * manufacturers for manufacturing new engines.  They need to
   * forecast the demand for a particular part, XC-288.
   */
  trait TrainParts extends org.specs2.specification.Scope {
    val csv = getClass.getClassLoader.getResource("TrainMax_Data.csv").getFile
    val data: DenseMatrix[Double] =
      breeze.linalg.csvread(new java.io.File(csv), skipLines = 1)
    val periods = data(::, 0)
    val demand = data(::, 1)
  }

  /**
   * Part 1
   * 
   * What is the coefficient of variation of the demand?
   */
  "W2PP4P1" in {
    new TrainParts {
      val MeanAndVariance(mean, variance, _) =
        breeze.stats.meanAndVariance(demand)
      // Coefficient of variation:
      val stddev = Math.sqrt(variance) 
      val cv = stddev/mean
      cv must beCloseTo(0.057, 2.significantFigures)
    }
  }

  /**
   * Part 2A
   * 
   * What is the forecast for demand in time period 15 using a Naive
   * model?
   */
  "W2PP4P2A" in {
    new TrainParts {
      demand(13) === 1169
    }
  }

  /**
   * Part 2B
   * 
   * What is the forecast for demand in time period 15 using a
   * Cumulative model?
   */
  "W2PP4P2B" in {
    new TrainParts {
      breeze.stats.mean(demand) must be ~(1104.0 +/- 0.5)
    }
  }

  /**
   * Part 2C
   * 
   * What is the forecast for demand in time period 15 using a 2
   * Period Moving Average model?
   */
  "W2PP4P2C" in {
    new TrainParts {
      val horizon = 2
      val movingForecast = DenseVector.tabulate(demand.length - horizon + 1) {
        i => breeze.stats.mean(demand(i to i + horizon - 1))
      }
      movingForecast(14 - horizon) must be ~(1145.0  +/- 0.5)
    }
  }

  /**
   * Part 2D
   * 
   * What is the forecast for demand in time period 15 using a 4
   * Period Moving Average model?
   */
  "W2PP4P2D" in {
    new TrainParts {
      val horizon = 4
      val movingForecast = DenseVector.tabulate(demand.length - horizon + 1) {
        i => breeze.stats.mean(demand(i to i + horizon - 1))
      }
      movingForecast(14 - horizon) must be ~(1113.0 +/- 0.5)
    }
  }

  /**
   * Part 3A
   * 
   * Create next period forecasts for the data for time periods 1
   * through 14 using the Naive, Cumulative, 2 Period Moving Average,
   * and 4 Period Moving Average models.
   * 
   * What is the MAPE for the next period Naive model?
   */
  "W2PP4P3A" in {
    new TrainParts {
      val naiveForecast = demand(0 to -2)
      val actual = demand(1 to -1)
      val forecastError = actual - naiveForecast
      val absolutePercentageError = breeze.numerics.abs(forecastError :/ actual)
      val mape = breeze.stats.mean(absolutePercentageError)
      mape must be ~(0.0708 +/- 0.0001)
    }
  }

  /**
   * Part 3B
   * 
   * What is the MAPE for the next period Cumulative model?
   */
  "W2PP4P3B" in {
    new TrainParts {
      val cumulativeForecast =
        DenseVector.tabulate(demand.length) {
          i => breeze.stats.mean(demand(0 to i))
        }
      val actual = demand(1 to -1)
      val forecastError = actual - cumulativeForecast(0 to -2)
      val absolutePercentageError = breeze.numerics.abs(forecastError :/ actual)
      val mape = breeze.stats.mean(absolutePercentageError)
      mape must be ~(0.0513 +/- 0.0001)
    }
  }

  /**
   * Part 3C
   * 
   * What is the MAPE for the next period 2 Period Moving Average
   * model?
   */
  "W2PP4P3C" in {
    new TrainParts {
      val horizon = 2
      val movingForecast = DenseVector.tabulate(demand.length - horizon + 1) {
        i => breeze.stats.mean(demand(i to i + horizon - 1))
      }
      val actual = demand(horizon to -1)
      val forecastError = actual - movingForecast(0 to -2)
      val absolutePercentageError = breeze.numerics.abs(forecastError :/ actual)
      val mape = breeze.stats.mean(absolutePercentageError)
      mape must be ~(0.0646 +/- 0.0001)
    }
  }

  /**
   * Part 3D
   * 
   * What is the MAPE for the next period 4 Period Moving Average
   * model?
   */
  "W2PP4P3D" in {
    new TrainParts {
      val horizon = 4
      val movingForecast = DenseVector.tabulate(demand.length - horizon + 1) {
        i => breeze.stats.mean(demand(i to i + horizon - 1))
      }
      val actual = demand(horizon to -1)
      val forecastError = actual - movingForecast(0 to -2)
      val absolutePercentageError = breeze.numerics.abs(forecastError :/ actual)
      val mape = breeze.stats.mean(absolutePercentageError)
      mape must be ~(0.0479 +/- 0.0001)
    }
  }
}
