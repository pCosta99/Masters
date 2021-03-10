public class Account {

    private final int id;
    private int balance;

    public Account(int id){
        this.id = id;
        balance = 0;
    }

    public int balance(){
        return balance;
    }

    public boolean deposit(int value){
        balance += value;
        return true;
    }

    public boolean withdraw(int value){
        if (value <= balance){
            balance -= value;
            return  true;
        } else {
            return false;
        }
    }

    public int getId(){
        return id;
    }
}
