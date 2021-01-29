package view;

public class EscolheLoja {

    private final String loja;
    private final String aceita;
    //private final boolean especial;


    public EscolheLoja(String loja,String aceita) {
        this.loja = loja;
        this.aceita = aceita;
        //this.especial = especial;
    }



    public String getLoja() {
        return loja;
    }

    public String getAceita() {
        return aceita;
    }


}
