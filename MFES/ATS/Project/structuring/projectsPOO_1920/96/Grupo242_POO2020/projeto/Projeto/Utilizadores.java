package Projeto;

import java.util.HashMap;
import java.util.Map;

public class Utilizadores {
    private Map<String, Utilizador> database;

    public Utilizadores(){
        this.database = new HashMap<>();
    }

    public Utilizador getUtilizador(String key){
        return database.get(key);
    }

    public void addUtilizador(Utilizador user){
        this.database.putIfAbsent(user.getEmail(),user.clone());
    }

    public boolean equals(Object pass){
        if (this == pass) return true;
        else return false;
    }

    @Override
    public String toString() {
        return "Utilizadores{" +
                "database=" + database +
                '}';
    }
}
