import scala.io.StdIn.{readLine, readInt}
import scala.io.Source
import java.io.{File, PrintWriter}

object bank{
    
    val filename = "users.txt"
    var account_no = Source.fromFile(filename).getLines.toList(0).split(",").toList.map((s: String) => s.toInt)
    var balance = Source.fromFile(filename).getLines.toList(1).split(",").toList.map((s: String) => s.toInt)
    var password = Source.fromFile(filename).getLines.toList(2).split(",").toList.map((s: String) => s.toInt)
    var credit_score = Source.fromFile(filename).getLines.toList(3).split(",").toList.map((s: String) => s.toInt)

    def approve_loan(acc_no : Int, amount: Int): Boolean = {
        var index = account_no.indexOf(acc_no)
        if (credit_score(index) < 650 ){
            return false
        }
        else if (credit_score(index) >= 650 && credit_score(index) < 750){
            if (amount > 5000){
                return false
            }
            else return true
        }
        else if (credit_score(index) >= 750 && credit_score(index) < 850){
            if (amount > 10000){
                return false
            }
            else return true
        }
        else if (credit_score(index) >= 850 && credit_score(index) < 950){
            if (amount > 15000){
                return false
            }
            else return true
        }
        else return true
    } 

    def total_funds():Int = {
        return balance.foldLeft(0)(_ + _)
    }

    def transfer(from: Int, to: Int, amount: Int): Unit = {
        var index1 = account_no.indexOf(from)
        var index2 = account_no.indexOf(to)
        if(amount > balance(index1)){
            println("Insufficient balance")
        }
        else{
            println("Enter password: ")
            var pass = readLine().toInt
            if(password(index1) == pass){
            balance = balance.updated(index1, balance(index1) - amount)
            balance = balance.updated(index2, balance(index2) + amount)
            println("Transfer successful")
            }
            else{
                println("Incorrect password")
            }
        }
    }
    def deposit(acc : Int, bal: Int, amount: Int): Int = {
        println("Enter password: ")
        var pass = readLine().toInt
        var index = account_no.indexOf(acc)
        if(password(index) == pass){
            println("")
            println("Amount deposited: " + amount)
            return bal + amount
        }
        else{
            println("Incorrect password")
            return bal
        }
    }

    def withdraw(acc : Int, bal: Int, amount: Int): Int = {
        println("Enter password: ")
        var pass = readLine().toInt
        var index = account_no.indexOf(acc)
        if(password(index) == pass){
            println("")
            println("Amount withdrawn: " + amount)
            return bal - amount
            
        }
        else{
            println("Incorrect password")
            return bal
        }
    }

    def update(accno : Int , amount: Int, f: (Int,Int,Int) => Int): Unit = {
        var index = account_no.indexOf(accno)
        balance = balance.updated(index, f(accno, balance(index), amount))
        println("Balance: " + balance(index))
        println("")
    }

    def user(accno : Int)={
        var finish = false
        var acc = accno
        while (!finish){
            println("1. Deposit")
            println("2. Withdraw")
            println("3. Account Details")
            println("4. Transfer Money")
            println("5. Exit")
            var choice = readLine("Enter your choice: ").toInt
            choice match {
                case 1 => {
                    println("Enter the amount to deposit: ")
                    var amt = readLine().toInt
                    var index = account_no.indexOf(acc)
                    if (index != -1){
                        update(acc, amt, deposit)
                    }
                    else{
                        println("Account number not found")
                    }
                }
                case 2 => {
                    println("Enter the amount to withdraw: ")
                    var amt = readLine().toInt
                    var index = account_no.indexOf(acc)
                    if (index != -1){
                        if (balance(index) >= amt){
                            update(acc, amt, withdraw)
                        }
                        else{
                            println("Insufficient balance")
                        }
                    }
                    else{
                        println("Account number not found")
                    }
                }
                case 3 => {
                    var index = account_no.indexOf(acc)
                    if (index != -1){
                        println("")
                        println("Account number: " + acc)
                        println("Balance: " + balance(index))
                        println("Credit Score: " + credit_score(index))
                        println("")5
                    }
                    else{
                        println("Account number not found")
                    }
                }
                case 4 => {
                    println("Enter the account number to transfer: ")
                    var accno = readLine().toInt
                    var index = account_no.indexOf(accno)
                    if (index != -1){
                        println("Enter the amount to transfer: ")
                        var amt = readLine().toInt
                        transfer(acc, accno, amt)
                    }
                    else{
                        println("Account number not found")
                    }
                }
                case 5 => {
                    finish = true
                }
                case _ => {
                    println("Invalid choice")
                }
            }
        }
    }

    def create_user() = {
        println("Enter the account number: ")
        var acc = readLine().toInt
        println("Enter the balance: ")
        var amt = readLine().toInt
        println("Enter the password: ")
        var pass = readLine().toInt
        account_no = account_no :+ acc
        balance = balance :+ amt
        password = password :+ pass
        credit_score = credit_score :+ 550
        println("User added successfully")
        println(account_no)
    }

    def delete_user() = {
        println("Enter the account number: ")
        var acc = readLine().toInt
        var index = account_no.indexOf(acc)
        if (index != -1){
            account_no = account_no.updated(index, 0)
            balance = balance.updated(index, 0)
            password = password.updated(index, 0)
            credit_score = credit_score.updated(index, 0)
            println("User deleted successfully")
        }
        else{
            println("Account number not found")
        }
        println(account_no)
    }

    def update_user() = {
        println("Enter the account number: ")
        var acc = readLine().toInt
        var index = account_no.indexOf(acc)
        if (index != -1){
            println("Enter the new password: ")
            var pass = readLine().toInt
            password = password.updated(index, pass)
            println("User updated successfully")
        }
        else{
            println("Account number not found")
        }
        println(account_no)
    }

    def admin()={
        var finish = false
        while(!finish){
            println("1. Add user")
            println("2. Delete user")
            println("3. Change password")
            println("4. Lend Loan")
            println("5. Total Funds")
            println("6. Exit")
            var choice = readLine("Enter your choice: ").toInt
            choice match {
                    case 1 => {
                          create_user()  
                        }
                    case 2 => {
                           delete_user() 
                        }
                    case 3 => {
                            update_user()
                        }
                    case 4 => {
                                println("Enter the account number: ")
                                var acc = readLine().toInt
                                println("Enter the amount to lend: ")
                                var amt = readLine().toInt
                                var index = account_no.indexOf(acc)
                                if (index != -1){
                                    if (approve_loan(acc, amt)){
                                        balance = balance.updated(index, balance(index) + amt)
                                        println("Loan granted successfully")
                                        println("Balance: " + balance(index))
                                    }
                                    else{
                                        println("Loan criterion not met")
                                    }
                                }
                                else{
                                    println("Account number not found")
                                }
                            
                    }
                    case 5 => {
                                println("")
                                println("Total funds: " + total_funds())
                                println("")
                            
                    }
                    case 6 => {
                                finish = true
                        }
                    case _ => {
                                println("Invalid choice")
                        }
                }
        }
        
    }

    def main(args: Array[String])= {
               
        println("Welcome to the Bank of Scala")
        var finish = false
        while (!finish){
            println("Make your choice")
            println("1. User")
            println("2. Admin")
            println("3. Exit")
            var choice = readLine("Enter your choice: ").toInt
            choice match {
                case 1 => {
                    println("Enter your account number: ")
                    var acc = readLine().toInt
                    var index = account_no.indexOf(acc)
                    if (index != -1){
                        println("Enter your password: ")
                        var pass = readLine().toInt
                        if (password(index) == pass){
                            user(acc)
                        }
                        else{
                            println("Invalid password")
                        }
                    }
                    else{
                        println("Account number not found")
                    }
                }
                case 2 => {
                    println("Enter your adminid: ")
                    var acc = readLine().toInt
                    println("Enter your password: ")
                    var pass = readLine().toInt
                    if (acc == 1 && pass == 1){
                        admin()
                    }
                }
                case 3 => {
                    finish = true
                }
                case _ => {
                    println("Invalid choice")
                }
            }
        }
        println("Thank you for using the Bank of Scala")
        var acc_fin = for (x <- account_no if x != 0) yield x
        var bal_fin = for (x <- balance if x != 0) yield x
        var pass_fin = for (x <- password if x != 0) yield x
        var cred_fin = for (x <- credit_score if x != 0) yield x

        val writer = new PrintWriter(new File("users.txt" ))
        writer.write(acc_fin.mkString(",") + "\n")
        writer.write(bal_fin.mkString(",") + "\n")
        writer.write(pass_fin.mkString(",") + "\n")
        writer.write(cred_fin.mkString(",") + "\n")
        writer.close()
    }
}