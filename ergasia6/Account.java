import java.util.concurrent.locks.ReentrantLock;

class Account {
    private final int ID;
    private volatile int balance; 
    private final ReentrantLock lock = new ReentrantLock();

    Account(int id, int balance) {
        this.ID = id;
        this.balance = balance;
    }

    int getId() {
        return ID;
    }

    int getBalance() {
        return balance;
    }

    void setBalance(int balance) {
        lock.lock();
        try {
            this.balance = balance;
        } finally {
            lock.unlock();
        }
    }

    void adjustBalance(int amount) {
        lock.lock();
        try {
            this.balance += amount;
        } finally {
            lock.unlock();
        }
    }
}
