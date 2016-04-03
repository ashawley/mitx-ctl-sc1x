/**
 * MITx: CTL.SC1x Supply Chain Fundamentals
 *
 * Week 1: Practice Problems
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

import breeze.linalg._
import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Poisson

import org.specs2.mutable.Specification

class Week1Spec extends Specification {

  /**
   * Practice Problem 2:  Uncertainty in Supply Chains
   *
   * Suppose that daily demand for bagels at the local coffee shop
   * where you work is found to be Normally distributed with a mean of
   * 250 and a standard deviation of 75 units.
    */
  trait Bagels extends org.specs2.specification.Scope {
    val mu                 = 250d // Mean
    val sigma              = 75d // Standard deviation
    val normalDistribution = Gaussian(mu, sigma) // Normal distribution
  }

  /**
   * Part 1
   *
   * Suppose you have 350 bagels ready to sell on a certain day. What
   * is the probability that you will run out? That is, what is the
   * probability that the demand is greater than 350 on a certain day?
   */
  "W1PP2P1" in {
    new Bagels {
      val suppose = 350d

      // Cumulative distribution function (CDF):
      val probabilityRunningOut = 1d - normalDistribution.cdf(suppose)

      // Check:
      probabilityRunningOut must beCloseTo(0.0912, 3.significantFigures)
    }
  }

  /**
   * Part 2
   *
   * This is not good enough for you! How many bagels do you need to
   * prepare to have only 5% probability of running out?
   */
  "W1PP2P2" in {
    new Bagels {
      val probabilityRunningOut = 0.05d

      // Inverse Cumulative distribution function (ICDF):
      val suppose = normalDistribution.icdf(1d - probabilityRunningOut)

      // Check:
      suppose must be ~(373.0 +/- 0.5)
    }
  }

  /**
   * Part 3
   *
   * You want to be even surer that you do not run out of bagels! How
   * many bagels do you need to prepare to have only 1% probability of
   * running out?
   */
  "W1PP2P3" in {
    new Bagels {
      val probabilityRunningOut = 0.01d
      // Inverse Cumulative distribution function (ICDF):
      val suppose = normalDistribution.icdf(1d - probabilityRunningOut)

      // Check:
      suppose must be ~(424.0 +/- 0.5)
    }
  }

  /**
   * Practice Problem 3: Three Month Salary Stores
   *
   * 3-Month Salary (3MS) is a high-end retailer of engagement rings
   * for the Greater Boston Region. They carry around 75 different
   * types of engagement ring mountings and 25 different types of
   * diamonds, for a total of 100 SKUs. They have traditionally kept
   * their inventory in a vault in their flagship store in downtown
   * Boston. However, as real estate prices rise and their sales
   * volume increases, they are considering moving the safety
   * inventory of all but the most profitable SKUs to a safe warehouse
   * in the nearby town of Watertown, where real estate is somewhat
   * cheaper. The company has decided to implement an ABC segmentation
   * of their SKUs, and it has asked for your help.
    */
  trait Rings extends org.specs2.specification.Scope {
    val csvFile: java.io.InputStream =
      getClass.getClassLoader.getResourceAsStream("w1pp3_sku3ms.csv")
    val csv: IndexedSeq[IndexedSeq[String]] =
      breeze.io.CSVReader.read(new scala.io.BufferedSource(csvFile).reader)
    val x: IndexedSeq[IndexedSeq[String]] =
      csv.drop(1).map(_.slice(1,4))
    val productCostPriceSold: DenseMatrix[Double] =
      DenseMatrix.tabulate(x.length,3)((i,j)=>x(i)(j).toDouble)

    val cost: DenseVector[Double]   = productCostPriceSold(::, 0)
    val price: DenseVector[Double]  = productCostPriceSold(::, 1)
    val sold: DenseVector[Double]   = productCostPriceSold(::, 2)
    val margin: DenseVector[Double] = price - cost
    val profit: DenseVector[Double] = margin :* sold

    val profitSorted: Array[Double] =
      profit.toArray.sorted.reverse
    val profitAccumPct: DenseVector[Double] =
      accumulate(DenseVector(profitSorted) :/ sum(profit))
    val profitAndPct: Array[(Double, Double)] =
      profitSorted.zip(profitAccumPct.toArray)

    /**
     * The company has decided to do this segmentation based on
     * profitability: type A products are those that are most
     * profitable, bringing the retailer a 34% of its monthly profit;
     * type B products are those that bring the next 37% of the profit;
     * and type C are the rest. In the linked file (w1pp3_sku3ms.xls),
     * you will find the information you need regarding the monthly
     * sales per SKU, along with their price and cost.
     */
    val group: Map[String, DenseVector[Double]] = profitAndPct.groupBy {
      // Group by percentage:
      case (_, p) if p <= .34       => "A"
      case (_, p) if p <= .34 + .37 => "B"
      case (_, p) if p > .34 + .37  => "C"
        // Only keep the product's profit value:
    }.mapValues(_.map(_._1)).mapValues(DenseVector(_))
  }

  /**
   * What is the least profitable of the type A products?
   */
  "W1PP3P1" in {
    new Rings {
      min(group("A")) must be ~(11889.9 +/- 0.1)
    }
  }

  /**
   * What type of product is SKU-3MS-507011?
   */
  "W1PP3P2" in {
    new Rings {
      group.exists {
        case (g, v) => v.findAll(_ == 7743.621).nonEmpty
      }
    }
  }

  /**
   * Practice Problem 5: Cakes at the Bagel Shop
   *
   * At the bagel shop, you have some cakes that you sell that are
   * pretty slow movers.  The chocolate cake, for example, only sells on
   * average 2.5 cakes per week.  You have noticed that the demand
   * follows a Poisson distribution.
   */
  trait Cakes extends org.specs2.specification.Scope {
    val lambda = 2.5d
    val poissonDistribution = Poisson(lambda)
  }

  /**
   * What is the probability that you will sell more than 3 cakes in a
   * week?
   */
  "W1PP5P1" in {
    new Cakes {
      val suppose = 3

      // Cumulative distribution function (CDF):
      val probabilityRunningOut = 1d - poissonDistribution.cdf(suppose)

      // Check:
      probabilityRunningOut must beCloseTo(0.24, 2.significantFigures)
    }
  }

  /**
   * What is the probability that you will sell no cakes in a week?
   */
  "W1PP5P2" in {
    new Cakes {
      val suppose = 0

      // Cumulative distribution function (CDF):
      val probabilityRunningOut = poissonDistribution.cdf(suppose)

      // Check:
      probabilityRunningOut must beCloseTo(0.082, 2.significantFigures)
    }
  }

  /**
   * Suppose you have 5 cakes made ready to sell. What is the
   * probability that you will sell out?
   */
  "W1PP5P3" in {
    new Cakes {
      val suppose = 5 - 1

      // Cumulative distribution function (CDF):
      val probabilityRunningOut = 1d - poissonDistribution.cdf(suppose)

      // Check:
      probabilityRunningOut must beCloseTo(0.11, 2.significantFigures)
    }
  }

  /**
   * Practice Problem 7: Uncertainty at Dog Co
   *
   * You have joined the supply chain group at Dog Co - the leading dog
   * supplies and food store operating in the United States.  You
   * discover that prior to your arrival all inventory and stocking
   * decisions were being made on just the average weekly demand.  The
   * distribution of the demand was being tracked, but it was not being
   * used at all in any of the inventory calculations.
   *
   * Your manager, Hank, however, does not think you need to consider
   * the distribution of demand.  "The average has always been good
   * enough for me!" he proclaims often and loudly.  He prefers to use
   * the average demand and then add in 10% on top, "Just to be safe".
   * This is known as "Hank's Rule" at Dog Co.
   *
   * You do not want to get fired, but you would like to demonstrate to
   * Hank that the distribution of demand does matter and perhaps
   * "Hank's Rule" should be changed.
   */
  trait Toys extends org.specs2.specification.Scope {
    val mu                 = 625d // Mean
    val sigma              = 225d // Standard deviation
    val normalDistribution = Gaussian(mu, sigma)
  }

  trait Treats extends org.specs2.specification.Scope {
    val mu                 = 630d // Mean
    val sigma              = 50d // Standard deviation
    val normalDistribution = Gaussian(mu, sigma)
  }

  /**
   * Part 1
   *
   * You identified two SKUs that have common average weekly demand, but
   * very different standard deviations.
   *
   * SKU #87990_A (Wilson Chew Toys) cost $5.95 each and you sell them
   * for $9.99. The weekly demand is distributed normally with a mean of
   * 625 units and a standard deviation of 225 units.
   *
   * SKU #333_99_J_4 (Dexter Delight Dog Treats) cost $4.25 and you sell
   * them for $8.00. The weekly demand is distributed normally with a
   * mean of 630 units and a standard deviation of 50 units.
   *
   * If you stocked exactly the mean of the demand for any one of the
   * items (that is 625 of the Wilson Chew Toys or 630 of the Dexter
   * Delights), what is the probability that your demand will exceed
   * what you have in stock for that item?
   */
  "W1PP7P1" in {
    new Toys {
      val suppose = mu

      // Inverse Cumulative distribution function (CDF):
      val probability = 1d - normalDistribution.cdf(suppose)

      // Check:
      probability must beCloseTo(0.5, 2.significantFigures)
    }

    new Treats {
      val suppose = mu

      // Cumulative distribution function (CDF):
      val probability = 1d - normalDistribution.cdf(suppose)

      // Check:
      probability must beCloseTo(0.5, 2.significantFigures)
    }
  }

  /**
   * Part 2
   *
   * Using the same two SKUs, you want to see how Hank's Rule applies to
   * each of the products and how it impacts the probability of stocking
   * out.
   *
   * What is the probability that the weekly demand for the Wilson Chew
   * Toys (only one SKU not both) will exceed the level set by Hanks
   * Rule, that is setting the inventory level at 688 (= 625 + 62.5)?
   */
  "W1PP7P2" in {
    new Toys {
      val suppose = mu + 0.1 * mu

      // Cumulative distribution function (CDF):
      val probability = 1d - normalDistribution.cdf(suppose)

      // Check:
      probability must beCloseTo(0.39, 2.significantFigures)
    }
  }

  /**
   * Part 3
   *
   * Using the same two SKUs, you want to see how Hank's Rule applies to
   * each of the products and how it impacts the probability of stocking
   * out.
   *
   * What is the probability that the weekly demand for the Dexter
   * Delights Dog Treats will exceed the level set by Hanks Rule, that
   * is setting the inventory level at 693 (= 630 + 63)?
   */
  "W1PP7P3" in {
    new Treats {
      val suppose = mu + 0.1 * mu

      // Cumulative distribution function (CDF):
      val probability = 1d - normalDistribution.cdf(suppose)

      // Check:
      probability must beCloseTo(0.1, 2.significantFigures)
    }
  }

  /**
   * Part 4
   *
   * Hank kind of believes you, but he says that that only works on the
   * fast moving items. He applies a different rule to slow moving items
   * called "Hank's Slow Mover Rule." This rule says that if average
   * weekly demand is less than 10, then stock the average, plus 1
   * unit. You pick two slow moving items for a test:
   *
   * SKU #11_9 (Griffin's Dog Bed) cost $195.95 each and you sell them
   * for $275. The weekly demand is Poisson distributed with a mean of 3
   * units.
   *
   * SKU #3765 (Cody Indestructible Chewables) cost $35.50 and you sell
   * them for $78.00. The weekly demand is Poisson distributed with a
   * mean of 6 units.
   */
  trait Beds extends org.specs2.specification.Scope {
    val lambda              = 3d
    val poissonDistribution = Poisson(lambda)
  }

  trait Chews extends org.specs2.specification.Scope {
    val lambda              = 6d
    val poissonDistribution = Poisson(lambda)
  }

  /**
   * What is the probability that your demand for Griffin Beds will
   * exceed the level set by Hank of 4 units (=3+1)?
   */
  "W1PP7P4" in {
    new Beds {
      val suppose = lambda.toInt + 1

      // Cumulative distribution function (CDF):
      val probabilityRunningOut = 1d - poissonDistribution.cdf(suppose)

      // Check:
      probabilityRunningOut must beCloseTo(0.18, 2.significantFigures)
    }
  }

  /**
   * Part 5
   *
   * What is the probability that your demand for Cody Indestructible
   * Chewables will exceed the level set by Hank of 7 units (=6+1)? Just
   * enter a number from 0 to 1.00 for the probability.
   */
  "W1PP7P5" in {
    new Chews {
      val suppose = lambda.toInt + 1

      // Cumulative distribution function (CDF):
      val probabilityRunningOut = 1d - poissonDistribution.cdf(suppose)

      // Check:
      probabilityRunningOut must beCloseTo(0.26, 2.significantFigures)
    }
  }
}
