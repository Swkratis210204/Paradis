import java.util.List;
import java.util.ArrayList;

class Bank {
	private final List<Account> accounts = new ArrayList<Account>();
	
	synchronized int newAccount(int balance) {
		int accountId = accounts.size(); 
		accounts.add(new Account(accountId, balance)); 
		return accountId;
	}
	
	synchronized Account getAccount(int accountId) {
		Account account = accounts.get(accountId);
		return account;
	}
}
