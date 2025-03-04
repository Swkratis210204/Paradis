import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.locks.ReentrantLock;

class Transaction implements Runnable {
    private final ConcurrentLinkedQueue<Operation> operations = new ConcurrentLinkedQueue<>();
    private volatile boolean closed = false;
    private final ReentrantLock lock = new ReentrantLock();

    void add(Operation operation) {
        lock.lock();
        try {
            if (closed) return;
            operations.add(operation);
        } finally {
            lock.unlock();
        }
    }

    void close() {
        closed = true;
    }

    public void run() {
        if (!closed) return;

        // Execute the operations.
        for (Operation operation : operations) {
            operation.run();
        }
    }
}
