import java.util.ArrayList;
import java.util.List;

class Transaction implements Runnable {
    private final List<Operation> operations = new ArrayList<>();
    private boolean closed = false;

    synchronized void add(Operation operation) {
        if (closed) return;
        operations.add(operation);
    }

    synchronized void close() {
        closed = true;
    }

    public void run() {
        synchronized (this) {
            if (!closed) return;
        }

        for (Operation operation : operations) {
            operation.run();
        }
    }
}
