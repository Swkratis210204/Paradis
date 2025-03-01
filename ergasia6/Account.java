// Peter Idestam-Almquist, 2023-02-26.

package paradis.assignment2;

class Account {
	// Instance variables.
	private final int ID;
	private int balance;
	
	// Constructor.
	Account(int id, int balance) {
		ID = id;
		this.balance = balance;
	}
	
	// Instance methods.
	
	int getId() {
		return ID;
	}
	
	int getBalance() {
		return balance;
	}
	
	void setBalance(int balance) {
		this.balance = balance;
	}
}
