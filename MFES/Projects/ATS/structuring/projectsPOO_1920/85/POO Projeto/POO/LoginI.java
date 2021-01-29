public interface LoginI {
    String getCodigo();
    void setCodigo(String codigo);
    String getNome();
    void setNome(String nome);
    String getPassword();
    void setPassword(String password);
    Login clone();
    boolean equals(Object o);
    String toString();
    void leTA(String cod, String[] p);
}
