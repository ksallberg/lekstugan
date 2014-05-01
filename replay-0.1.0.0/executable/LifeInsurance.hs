import Cgi

-- | Main to compile
main :: IO ()
main = runCGI insurance

-- | Ask some questions about the users
--   personal life in order to calculate
--   a proper annual life insurance cost
insurance :: Cgi ()
insurance = do
   -- | Ask about personal details
   details <- ask [(1,"First name?",IsString),
                   (2,"Surname?",IsString),
                   (3,"Age?",IsInt),
                   (4,"Number of family members?",IsInt)
                  ]
   -- | Ask about dangerous pets able to kill the client
   risk    <- ask [(5,"How many hours a week are you in a car?",IsInt),
                   (6,"How many dangerous pets do you have?",IsInt),
                   (7,"How many of them are poisonous?",IsInt),
                   (8,"How many of them are dinosaurs?",IsInt),
                   (9,"How many of them live in your car?",IsInt)
                  ]
   -- | Ask about potential family tensions
   personal <- ask [(10,"How many of your family members love " ++
                        "Texas Chainsaw massacre?",IsInt),
                    (11,"How many chainsaws do you have easily accessible "
                        ++ "in your garage?",IsInt)
                   ]
   -- | Results page (calculating the price)
   liftIO (putStrLn $
      -- | Intro of the results page
      "<h1>Dear, "
      ++ pickStr 1 details 
      ++ " " ++ pickStr 2 details ++ "</h1>"
      ++ "<strong>We are glad you completed " 
      ++ "Life Insurance Pricing Calculator 2000 Basic."
      ++ "</strong><br/><br/>"
      ++ "Please see the following pricing of your life insurance<br/>"
      ++ "<h2>Basic risk</h2>"
      ++ "<i>"
      ++ "$20 for each year your age = $"
         ++ show (20*pickInt 3 details)
      ++ "<br/>"
      ++ "$50 for each of your crazy family members = $"
         ++ show (50*pickInt 4 details)
      ++ "</i>"
      -- | Dangerous pets clause
      ++ "<h2>Additional cost of dangerous pets</h2>"
      ++ "<i>"
      ++ (pickStr 6 risk) ++ " dangerous pets a $30 = $"
         ++ show (30*pickInt 6 risk) ++ "<br/>"
      ++ (pickStr 7 risk) ++ " poisonous pets a $50 = $"
         ++ show (50*pickInt 7 risk) ++ "<br/>"
      ++ (pickStr 8 risk) ++ " dinousaurs a $200 = $"
         ++ show (200*pickInt 8 risk) ++ "<br/>"
      ++ (pickStr 9 risk) ++ " pets in your car * your time in the car = $"
         ++ show ((pickInt 5 risk)*(pickInt 9 risk)*100)
      ++ "(risk of geting stolen and dangerous)<br/>"
         ++ "</i>"
      -- | Family chainsaw risk clause
      ++ "<h2>Family centric risk</h2>"
      ++ "<i>"
      ++ "Family members loving Texas Chainsaw Massacre a $100 = $"
         ++ show (100*pickInt 10 personal) ++ "<br/>"
      ++ "Number of chainsaws * number of familymembers * 100 = $"
         ++ show ((pickInt 10 personal)*(pickInt 11 personal)*100)
      ++ "</i>"
      -- | Total cost
      ++ "<h2>Annual total cost of your life insurance:</h2>"
      ++ "<strong>$" ++ show (
            pickInt 3 details * 20  +
            pickInt 4 details * 50  +
            pickInt 6 risk    * 30  +
            pickInt 7 risk    * 50  +
            pickInt 8 risk    * 200 +
            ((pickInt 5 risk)*(pickInt 9 risk)*100) +
            (100*pickInt 10 personal) +
            ((pickInt 10 personal)*(pickInt 11 personal)*100) ) 
         ++ "</strong>"
         ++ "<br/>The money has been withdrawn from your bank account."
    )
   return ()
