package Model;

import java.util.Map;
import java.util.HashMap;
import java.io.Serializable;

public class Logins implements ILogins, Serializable {

	private Map<String,String> logins;

	public Logins() {
		this.logins = new HashMap<>();
	}

	public Logins(Map<String, String> logins) {
		setLogins(logins);
	}

	public Logins(Logins l) {
		setLogins(l.getLogins());
	}

	private Map<String, String> getLogins() {
		Map<String, String> aux = new HashMap<>();
		for(Map.Entry<String, String> l : this.logins.entrySet())
			aux.put(l.getKey(), l.getValue());
		return aux;
	}

	private void setLogins(Map<String,String> l) {
		this.logins = new HashMap<>();
		l.entrySet().forEach(aux -> this.logins.put(aux.getKey(), aux.getValue()));
	}

	@Override
	public void put(String username, String pw) {
		this.logins.put(username, pw);
	}

	@Override
	public boolean checkUserName(String username) {
		return (this.logins.containsKey(username));
	}

	@Override
	public boolean checkCredentials(String username, String pw) {
		return (this.logins.get(username).equals(pw));
	}

	@Override
	public String extractPassWord(String userName) {
		return this.logins.get(userName);
	}

	@Override
	public void changePassWord(String userName, String pw) {
		this.logins.replace(userName,pw);
	}

}