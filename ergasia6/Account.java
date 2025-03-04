import java.util.concurrent.atomic.AtomicInteger;

class Account {
    private final int ID;
    private final AtomicInteger balance;

    Account(int id, int balance) {
        this.ID = id;
        this.balance = new AtomicInteger(balance);
    }

    int getId() {
        return ID;
    }

    int getBalance() {
        return balance.get();
    }

    void setBalance(int balance) {
        this.balance.set(balance);
    }

    void adjustBalance(int amount) {
        this.balance.addAndGet(amount);
    }
}
