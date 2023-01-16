data Status 
    = Upcoming
    | Canceled
    | InProgress
    | Finished

--              destination price status [entertainment]
data Trip = Trip {destination::String, price::Double, tripStatus::Status, entertainments::[Activity]}

data Activity = Activity {nameActivity::String, duration::Int}

--                      name            points
data Customer = Customer {nameCustomer::String, excursions::[Trip], points::Int}

type Agency = [Customer]

data Thresholds = Thresholds { moneyThreshold :: Double, durationThreshold :: Int,
loyaltyPointsThreshold :: Int }

data CencelationPolicy
    = Flexible
    | Moderate
    | Strict

totalDurationTrip :: Trip -> Int
totalDurationTrip (Trip _ _ _ entList) = sum (map duration entList)

totalDurationCustomerTrips :: Customer -> Int
totalDurationCustomerTrips c = sum (map totalDurationTrip (filter timeSpentOnTrip (excursions c)))
    where
        timeSpentOnTrip (Trip _ _ Canceled _) = False
        timeSpentOnTrip (Trip _ _ Upcoming _) = False
        timeSpentOnTrip (Trip _ _ _ _) = True

totalPriceCustomerTrip :: Customer -> Double
totalPriceCustomerTrip c = sum (map price (filter moneySpentOnTrip (excursions c)))
    where
        moneySpentOnTrip (Trip _ _ Canceled _) = False
        moneySpentOnTrip (Trip _ _ _ _) = True

freeTripEligibleCustomers :: Agency -> Thresholds -> [Customer]
freeTripEligibleCustomers agency thresholds = [c | c<-agency, totalDurationCustomerTrips c >= durationThreshold thresholds,totalPriceCustomerTrip c >= moneyThreshold thresholds,points c >= loyaltyPointsThreshold thresholds]