{-
Bank Management System
-}

withdraw :: Int -> Int -> Int
withdraw amt bal = if amt > bal then 0 else bal - amt

deposit :: Int -> Int -> Int
deposit amt bal = bal + amt

update :: Int -> Int -> (Int -> Int -> Int) -> Int
update amt bal f = f amt bal

infi = do
    let account_no = [1,2,3,4,5,6,7,8,9,10]
    let balance = [100,200,300,400,500,600,700,800,900,1000]
    let password = [1,2,3,4,5,6,7,8,9,10]
    let finished = False
    print "Please enter your account number"
    inp1 <- getLine
    print "Please enter your password"
    inp2 <- getLine
    let acc_no = read inp1 :: Int
    let pass = read inp2 :: Int
    if acc_no `elem` account_no && pass == password !! (acc_no-1)
        then do
            print "Enter your choice"
            print "1. Withdraw"
            print "2. Deposit"
            print "3. Balance"
            print "4. Accounts without minimum balance"
            print "5. Add user"
            print "6. Account Details"
            print "7. Exit"
            inp3 <- getLine
            let choice = read inp3 :: Int
            if choice ==1
                then do
                    print "Enter the amount to withdraw"
                    inp4 <- getLine
                    let amt = read inp4 :: Int
                    let new_bal = balance !! (acc_no-1)
                    if withdraw amt new_bal == 0
                        then do
                            print "You do not have sufficient balance"
                            print "Please try again"
                        else do
                            print "Your new balance is"
                            print (update amt new_bal withdraw)
                            print "Thank you for banking with us"
                else if choice ==2
                    then do
                        print "Enter the amount to deposit"
                        inp5 <- getLine
                        let amt = read inp5 :: Int
                        let new_bal = balance !! (acc_no-1)
                        print "Your new balance is"
                        print (update amt new_bal deposit)
                        print "Thank you for banking with us"
                    else if choice ==3
                        then do
                            let new_bal1 = balance !! (acc_no-1)
                            print "Your balance is"
                            print new_bal1
                            print "Thank you for banking with us"
                        else if choice ==4
                            then do
                                print  "The Accounts without minimum balance are: "
                                print [x | x <- balance, x <400]
                                print "Thank you for banking with us"
                            
                                else if choice ==6
                                    then do
                                        print "Enter the account number"
                                        inp9 <- getLine
                                        let acc_no = read inp9 :: Int
                                        print "Enter the password"
                                        inp10 <- getLine
                                        let pass = read inp10 :: Int
                                        if acc_no `elem` account_no && pass == password !! (acc_no-1)
                                            then do
                                                print "Your account details are:"
                                                print "Account number:"
                                                print acc_no
                                                print "Balance:"
                                                print (balance !! (acc_no-1))
                                                print "Thank you for banking with us"
                                            else do
                                                print "Invalid account number or password"
                                                print "Please try again"
                                    else if choice ==7
                                        then do
                                            print "Thank you for banking with us"
                                        else do
                                            print "Invalid choice"
            else do print "Incorrect Password"
    infi

main = do
    print "Welcome to the bank"
    infi
    print "Thank you for banking with us"