import java.util.concurrent.locks.ReentrantReadWriteLock;

class Account {
    private final int ID;
    private int balance;
    private ReentrantReadWriteLock readWriteLock = new ReentrantReadWriteLock();

    // Constructor.
    Account(int id, int balance) {
        this.ID = id;
        this.balance = balance;
    }

    int getId() {
        return ID;
    }

    int getBalance() {
		readWriteLock.readLock().lock(); 
		try {
			return balance;
		} finally {
			readWriteLock.readLock().unlock();
		}
	}
	

    void setBalance(int balance) {
        readWriteLock.writeLock().lock();
        try {
            this.balance = balance;
        } finally {
            readWriteLock.writeLock().unlock();
        }
    }
}
