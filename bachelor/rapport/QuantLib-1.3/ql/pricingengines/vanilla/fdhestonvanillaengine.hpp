/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2008 Andreas Gaida
 Copyright (C) 2008 Ralph Schreyer
 Copyright (C) 2008, 2009 Klaus Spanderen

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

/*! \file fdhestonvanillaengine.hpp
    \brief Finite-Differences Heston vanilla option engine
*/

#ifndef quantlib_fd_heston_vanilla_engine_hpp
#define quantlib_fd_heston_vanilla_engine_hpp

#include <ql/instruments/dividendvanillaoption.hpp>
#include <ql/models/equity/hestonmodel.hpp>
#include <ql/pricingengines/genericmodelengine.hpp>
#include <ql/methods/finitedifferences/solvers/fdmsolverdesc.hpp>
#include <ql/methods/finitedifferences/solvers/fdmbackwardsolver.hpp>

namespace QuantLib {

    //! Finite-Differences Heston Vanilla Option engine

    /*! \ingroup vanillaengines

        \test the correctness of the returned value is tested by
              reproducing results available in web/literature
              and comparison with Black pricing.
    */
    class FdHestonVanillaEngine
        : public GenericModelEngine<HestonModel,
                                    DividendVanillaOption::arguments,
                                    DividendVanillaOption::results> {
      public:
        // Constructor
        FdHestonVanillaEngine(
            const boost::shared_ptr<HestonModel>& model,
            Size tGrid = 100, Size xGrid = 100, 
            Size vGrid = 50, Size dampingSteps = 0,
            const FdmSchemeDesc& schemeDesc = FdmSchemeDesc::Hundsdorfer());

        void calculate() const;
        
        // multiple strikes caching engine
        void update();
        void enableMultipleStrikesCaching(const std::vector<Real>& strikes);
        
        // helper method for Heston like engines
        FdmSolverDesc getSolverDesc(Real equityScaleFactor) const;

      private:
        const Size tGrid_, xGrid_, vGrid_, dampingSteps_;
        const FdmSchemeDesc schemeDesc_;
        
        std::vector<Real> strikes_;
        mutable std::vector<std::pair<DividendVanillaOption::arguments,
                                      DividendVanillaOption::results> >
                                                            cachedArgs2results_;
    };

}

#endif
