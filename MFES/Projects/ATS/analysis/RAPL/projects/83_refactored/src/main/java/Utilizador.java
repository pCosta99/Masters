import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Utilizador extends UtilizadorSistema implements Serializable {
      private List<Encomenda> encomendas_realizadas;

    //Construtores
      public Utilizador(){
          super();
          this.encomendas_realizadas = new ArrayList<>();
      }

      public Utilizador(String email, String password, String codigo, String nome, double latitude, double longitude, ArrayList<Encomenda> encomendas_realizadas){
          super(email,password,"Utilizador", codigo, nome, latitude, longitude);
          setEncomendas(encomendas_realizadas);
      }

      public Utilizador(Utilizador user){
          super(user);
          setEncomendas(user.getEncomendas());
      }

      //Getters
      public String getCodigo(){
        return super.getCodigo();
      }

      public String getNome(){
         return  super.getNome();
      }

      public double getLatitude(){
          return super.getLatitude();
      }

      public double getLongitude(){
          return super.getLongitude();
      }

      public ArrayList<Encomenda> getEncomendas(){
          ArrayList<Encomenda> res = new ArrayList<>();
          for(Encomenda e: this.encomendas_realizadas)
              res.add(e.clone());
          return res;
      }

    //Setters
      public void setCodigo(String codigo){
          super.setCodigo(codigo);
      }

      public void setNome(String nome){
          super.setNome(nome);
      }

      public void setLatitude(double latitude){
          super.setLatitude(latitude);
      }

      public void setLongitude(double longitude){
          super.setLongitude(longitude);
      }

      public void setEncomendas(ArrayList<Encomenda> enc){
          this.encomendas_realizadas = new ArrayList<>();
          for(Encomenda e: enc)
              this.encomendas_realizadas.add(e.clone());
      }

    //Clone
      public Utilizador clone(){
        return new Utilizador(this);
      }

      //Equals
      public boolean equals(Object o){
          return super.equals(o);
      }

      public String toString(){
          return super.toString() +
                  "Código: " + getCodigo() + "\n" +
                  "Nome: " + getNome() + "\n" +
                  "Latitude: " + getLatitude() + "\n" +
                  "Longitude: " + getLongitude() + "\n";
      }

    /**
     * Método que imprime as encomendas do utilizador
     */
      public String printEncomendasRecebidas(){
          StringBuilder sb = new StringBuilder();
          if(this.encomendas_realizadas.stream().noneMatch(Encomenda::isEntregue)) sb.append("Não existem encomendas recebidas\n");
          else {
              System.out.println("ENCOMENDAS REALIZADAS PELO USER: ");
              this.encomendas_realizadas.stream().filter(Encomenda::isEntregue).forEach(e -> sb.append(e.toString()));
          }
          return sb.toString();
      }

    public String printEncomendasPorEntregar(){
        StringBuilder sb = new StringBuilder();
        if(this.encomendas_realizadas.stream().allMatch(Encomenda::isEntregue)) sb.append("Não existem encomendas por entregar\n");
        else {
            System.out.println("ENCOMENDAS REALIZADAS PELO USER: ");
            this.encomendas_realizadas.stream().filter(e -> !e.isEntregue()).forEach(e -> sb.append(e.toString()));
        }
        return sb.toString();
    }

    /**
     * Método que adiciona uma encoemenda
     */
      public void addEncomenda(Encomenda e){
          this.encomendas_realizadas.add(e.clone());
      }

    public void updateEncomendaLoja(Encomenda enc){
        ArrayList<Encomenda> aux = new ArrayList<>();
        enc.setLevantada(true);
        aux.add(enc);
        for(Encomenda e: this.encomendas_realizadas){
            if(!e.getCodigo().equals(enc.getCodigo())){
                aux.add(e);
            }
        }
        setEncomendas(aux);
    }

    public void updateEncomendaPronta(Encomenda enc){
        ArrayList<Encomenda> aux = new ArrayList<>();
        enc.setPreparada(true);
        aux.add(enc);
        for(Encomenda e: this.encomendas_realizadas){
            if(!e.getCodigo().equals(enc.getCodigo())){
                aux.add(e);
            }
        }
        setEncomendas(aux);
    }

    /**
     * Método que atualiza uma encomenda
     */
    public void updateEncomenda(Encomenda enc){
        ArrayList<Encomenda> aux = new ArrayList<>();
        enc.setEntregue(true);
        aux.add(enc);
        for(Encomenda e: this.encomendas_realizadas){
            if(!e.getCodigo().equals(enc.getCodigo())){
                aux.add(e);
            }
        }
        setEncomendas(aux);
    }

    /**
     * Método que devolve uma encomenda com o código cod
     */
    public Encomenda devolveEncomenda(String cod) throws EncomendaNotFoundException{
        for(Encomenda e: this.encomendas_realizadas){
            if(e.getCodigo().equals(cod)) return e.clone();
        }
        throw new EncomendaNotFoundException();
    }
}
