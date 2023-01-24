data Status 
    = Upcoming
    | Canceled
    | InProgress
    | Finished
    deriving Eq

--              destination price status [entertainment]
data Trip = Trip {destination::String, price::Double, tripStatus::Status, entertainments::[Activity], policy::CancelationPolicy}

data Activity = Activity {nameActivity::String, duration::Int}

--                      name            points
data Customer = Customer {nameCustomer::String, excursions::[Trip], points::Int}

type Agency = [Customer]

data Thresholds = Thresholds { moneyThreshold :: Double, durationThreshold :: Int,
loyaltyPointsThreshold :: Int }

data CancelationPolicy
    = Flexible
    | Moderate
    | Strict

totalDurationTrip :: Trip -> Int
totalDurationTrip (Trip _ _ _ entList _) = sum (map duration entList)

totalDurationCustomerTrips :: Customer -> Int
totalDurationCustomerTrips c = sum (map totalDurationTrip (filter timeSpentOnTrip (excursions c)))
    where
        timeSpentOnTrip (Trip _ _ Canceled _ _) = False
        timeSpentOnTrip (Trip _ _ Upcoming _ _) = False
        timeSpentOnTrip _ = True

totalPriceCustomerTrip :: Customer -> Double
totalPriceCustomerTrip c = sum (map price (filter moneySpentOnTrip (excursions c)))
    where
        moneySpentOnTrip (Trip _ _ Canceled _ _) = False
        moneySpentOnTrip _ = True

freeTripEligibleCustomers :: Agency -> Thresholds -> [Customer]
freeTripEligibleCustomers agency thresholds = [c | c<-agency, totalDurationCustomerTrips c >= durationThreshold thresholds,totalPriceCustomerTrip c >= moneyThreshold thresholds,points c >= loyaltyPointsThreshold thresholds]

getRefund :: Agency -> String -> String -> Maybe Double
getRefund agency customerName destinationName
    | null goodCustomers = Nothing
    | null goodExcursion = Nothing
    | otherwise = case tripStatus excursion of
        Canceled -> Nothing
        Finished -> Nothing
        Upcoming -> Just $ calc $ excursion
        InProgress -> Just 0
    where 
        goodCustomers = filter (\c -> nameCustomer c == customerName) agency
        customer = head goodCustomers
        goodExcursion = filter (\t -> destination t == destinationName) (excursions customer)
        excursion = head goodExcursion
        calc :: Trip -> Double
        calc t = (k*c*price t) - fee
            where
                k = fromIntegral (length (filter (\trip -> tripStatus trip /= Canceled) (excursions customer))) / fromIntegral (length (excursions customer))
                c = case policy excursion of
                        Flexible -> 0.7
                        Moderate -> 0.4
                        Strict -> 0.1
                fee = quotient * (if null canceled then 0 else canceledAvg)
                quotient = fromIntegral (length haveCanceled) / fromIntegral (length allInTrip)
                canceledAvg =sum canceled / fromIntegral (length canceled)
                canceled = [price t | t<-excursions customer,tripStatus t == Canceled]
                haveCanceled = filter (any (\t -> destination t == destinationName && tripStatus t == Canceled) . excursions) agency
                allInTrip = filter (any (\t -> destination t == destinationName) . excursions) agency
                    

match = Activity "match" 30                                                
eating = Activity "eating" 35
shopping = Activity "shopping" 100                                         
museums = Activity "museums" 120                                           
paris = Trip "Paris" 1000 Upcoming [match,shopping] Flexible               
london = Trip "London" 750 InProgress [museums, match, eating] Strict      
madrid = Trip "Madrid" 1500 Upcoming [match, match, match,eating] Moderate 
sofia = Trip "Sofia" 200 InProgress [shopping , museums , shopping , match] Flexible
az = Customer "Kaloyan" [madrid, sofia] 100                                
ti = Customer "Dani" [london, paris,sofia] 78                              
toi = Customer "Vladi" [sofia, madrid] 132   
agency = [az, ti, toi]
requirements = Thresholds 1200 100 90 

main :: IO()
main = do
    putStr $ "Eligible for free trip: "
    print $ map nameCustomer $ freeTripEligibleCustomers agency requirements
    putStr $ "Refund for Kaloyan for Madrid: "
    print $ getRefund agency "Kaloyan" "Madrid"
    putStr $ "Refund for Kaloyan for madrid: "
    print $ getRefund agency "Kaloyan" "madrid"
    putStr $ "Refund for Kaloyan for Sofia: "
    print $ getRefund agency "Kaloyan" "Sofia"