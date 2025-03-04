import java.util.ArrayList;
import java.util.List;

class Bank {
    private final List<Account> accounts = new ArrayList<>();

    synchronized int newAccount(int balance) {
        int accountId = accounts.size();
        accounts.add(new Account(accountId, balance));
        return accountId;
    }

    Account getAccount(int accountId) {
        return accounts.get(accountId);
    }
}
