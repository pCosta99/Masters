package trazaqui;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class LogLoja extends Loja implements Serializable {
    private String username;
    private String password;
    private CatalogoProdutos cp;

    //getters
    public String getUsername(){return this.username;}

    public String getPassword(){return this.password;}

    public CatalogoProdutos getCatalogoProdutos(){return this.cp;}

    //setters
    public void setUsername(String user){this.username=user;}

    public void setPassword(String pass){this.password=pass;}

    public void setCatalogoProdutos(CatalogoProdutos catalogo){this.cp=catalogo;}

    //construtor vazio
    public LogLoja(){
        super();
        this.username="";
        this.password="";
        this.cp=new CatalogoProdutos();
    }

    //construtores parametrizados
    public LogLoja(String cod,String nome, Localizacao pos, String user, String pass,CatalogoProdutos catalogo){
        super(cod,nome,pos);
        this.username=user;
        this.password=pass;
        setCatalogoProdutos(catalogo);
    }

    public LogLoja(Loja l, String user, String pass,CatalogoProdutos catalogo){
        super(l.getCodLoja(),l.getNome(),l.getGps());
        this.username=user;
        this.password=pass;
        setCatalogoProdutos(catalogo);
    }

    //construtor por cópia
    public LogLoja(LogLoja l){
        super(l.getCodLoja(),l.getNome(),l.getGps());
        this.username=l.getUsername();
        this.password=l.getPassword();
        setCatalogoProdutos(l.getCatalogoProdutos());
    }

    //metodo String
    public String toString(){
        StringBuilder sb= new StringBuilder();
        sb.append(super.toString());
        sb.append("Username:").append(this.username).append("\n")
                .append("Password:").append(this.password).append("\n")
                .append("Produtos:").append(this.cp).append("\n");

        return sb.toString();
    }

    //metodo clone
    public LogLoja clone(){ return new LogLoja(this);}

    //metodo equals
    public boolean equals(Object o){
        if (o==this) return true;
        if ((o==null) || (o.getClass()!=this.getClass())) return false;

        LogLoja loj= (LogLoja) o;

        return super.equals(loj)
                && loj.getUsername().equals(this.getUsername())
                && loj.getPassword().equals(this.getPassword())
                && loj.getCatalogoProdutos().equals(this.getCatalogoProdutos());
    }

}
