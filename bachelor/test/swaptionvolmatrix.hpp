/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2006 Fran�ois du Vignaud
 Copyright (C) 2006, 2008 Ferdinando Ametrano

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

/*! \file swaptionvolmatrix.hpp
    \brief Swaption at-the-money volatility matrix
*/

#ifndef quantlib_swaption_volatility_matrix_hpp
#define quantlib_swaption_volatility_matrix_hpp

#include <ql/termstructures/volatility/swaption/swaptionvoldiscrete.hpp>
#include <ql/math/interpolations/interpolation2d.hpp>
#include <ql/math/matrix.hpp>
#include <boost/noncopyable.hpp>
#include <vector>

namespace QuantLib {

    class Quote;

    //! At-the-money swaption-volatility matrix
    /*! This class provides the at-the-money volatility for a given
        swaption by interpolating a volatility matrix whose elements
        are the market volatilities of a set of swaption with given
        option date and swapLength.

        The volatility matrix <tt>M</tt> must be defined so that:
        - the number of rows equals the number of option dates;
        - the number of columns equals the number of swap tenors;
        - <tt>M[i][j]</tt> contains the volatility corresponding
          to the <tt>i</tt>-th option and <tt>j</tt>-th tenor.
    */
    class SwaptionVolatilityMatrix : public SwaptionVolatilityDiscrete,
                                     private boost::noncopyable {
      public:
        //! floating reference date, floating market data
        SwaptionVolatilityMatrix(
                    const Calendar& calendar,
                    BusinessDayConvention bdc,
                    const std::vector<Period>& optionTenors,
                    const std::vector<Period>& swapTenors,
                    const std::vector<std::vector<Handle<Quote> > >& vols,
                    const DayCounter& dayCounter);
        //! fixed reference date, floating market data
        SwaptionVolatilityMatrix(
                    const Date& referenceDate,
                    const Calendar& calendar,
                    BusinessDayConvention bdc,
                    const std::vector<Period>& optionTenors,
                    const std::vector<Period>& swapTenors,
                    const std::vector<std::vector<Handle<Quote> > >& vols,
                    const DayCounter& dayCounter);
        //! floating reference date, fixed market data
        SwaptionVolatilityMatrix(
                    const Calendar& calendar,
                    BusinessDayConvention bdc,
                    const std::vector<Period>& optionTenors,
                    const std::vector<Period>& swapTenors,
                    const Matrix& volatilities,
                    const DayCounter& dayCounter);
        //! fixed reference date, fixed market data
        SwaptionVolatilityMatrix(
                    const Date& referenceDate,
                    const Calendar& calendar,
                    BusinessDayConvention bdc,
                    const std::vector<Period>& optionTenors,
                    const std::vector<Period>& swapTenors,
                    const Matrix& volatilities,
                    const DayCounter& dayCounter);
        // fixed reference date and fixed market data, option dates
        SwaptionVolatilityMatrix(const Date& referenceDate,
                                 const std::vector<Date>& optionDates,
                                 const std::vector<Period>& swapTenors,
                                 const Matrix& volatilities,
                                 const DayCounter& dayCounter);
        //! \name LazyObject interface
        //@{
        void performCalculations() const;
        //@}
        //! \name TermStructure interface
        //@{
        Date maxDate() const;
        //@}
        //! \name VolatilityTermStructure interface
        //@{
        Rate minStrike() const;
        Rate maxStrike() const;
        //@}
        //! \name SwaptionVolatilityStructure interface
        //@{
        const Period& maxSwapTenor() const;
        //@}
        //! \name Other inspectors
        //@{
        //! returns the lower indexes of surrounding volatility matrix corners
        std::pair<Size,Size> locate(const Date& optionDate,
                                    const Period& swapTenor) const {
            return locate(timeFromReference(optionDate),
                          swapLength(swapTenor));
        }
        //! returns the lower indexes of surrounding volatility matrix corners
        std::pair<Size,Size> locate(Time optionTime,
                                    Time swapLength) const {
            return std::make_pair(interpolation_.locateY(optionTime),
                                  interpolation_.locateX(swapLength));
        }
        //@}
      protected:
        // defining the following method would break CMS test suite
        // to be further investigated
        //boost::shared_ptr<SmileSection> smileSectionImpl(const Date&,
        //                                                 const Period&) const;
        boost::shared_ptr<SmileSection> smileSectionImpl(Time,
                                                         Time) const;
        Volatility volatilityImpl(Time optionTime,
                                  Time swapLength,
                                  Rate strike) const;
      private:
        void checkInputs(Size volRows,
                         Size volsColumns) const;
        void registerWithMarketData();
        std::vector<std::vector<Handle<Quote> > > volHandles_;
        mutable Matrix volatilities_;
        Interpolation2D interpolation_;
    };

    // inline definitions

    inline Date SwaptionVolatilityMatrix::maxDate() const {
        return optionDates_.back();
    }

    inline Rate SwaptionVolatilityMatrix::minStrike() const {
        return QL_MIN_REAL;
    }

    inline Rate SwaptionVolatilityMatrix::maxStrike() const {
        return QL_MAX_REAL;
    }

    inline const Period& SwaptionVolatilityMatrix::maxSwapTenor() const {
        return swapTenors_.back();
    }

    inline Volatility SwaptionVolatilityMatrix::volatilityImpl(Time optionTime,
                                                               Time swapLength,
                                                               Rate) const {
        calculate();
        return interpolation_(swapLength, optionTime, true);
    }

}

#endif
