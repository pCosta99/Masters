public class Bank implements BankInterface{

    private final Account account;

    public Bank(){
        this.account = new Account(0);
    }

    @Override
    public int balance() {
        return account.balance();
    }

    @Override
    public boolean movement(int value) {
        if (value > 0){
            return account.deposit(value);
        } else {
            return account.withdraw(-value);
        }
    }

    public void setBalance(int value) {
        account.deposit(value);
    }
}
