
/**
 * Escreva a descrição da classe LojaFE aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */

import java.util.Map;

public class LojaFE extends Loja
{
    private int tamanhoFila;
    private float tempoEspera;
    
//construtor vazio
   
    public LojaFE(){
    super();
    this.tamanhoFila = 0;
    this.tempoEspera = 0;
    }
    
//construtor parametrizado
    /*
    public LojaFE(String nome, String idLoja, float lat, float lon, Map<String,Encomenda> enc, float temp, int tam, float time){
    super(nome,idLoja,lat,lon,enc,temp);
    this.tamanhoFila = tam;
    this.tempoEspera = time;
    }*/
    
//construtor de copia
   /* 
    public LojaFE(LojaFE l){
    super(l.getNome(),l.getId(),l.getLat(),l.getLong(),l.getEnc(),l.getTemp());
    this.tamanhoFila = getTam();
    this.tempoEspera = getTempo();
    }*/
    
//metodo que devolve o tamanho médio da fila de espera da lojaFE em causa
    
    public int getTam(){
    return this.tamanhoFila;
    }
    
//metodo que devolve o tempo médio de espera da lojaFE em causa
    
    public float getTempo(){
    return this.tempoEspera;
    }
    
//metodo que da set ao tamanha médio da fila de espera da lojaFE em causa
    
    public void setTam(int t){
    this.tamanhoFila = t;
    }
    
//metodo que da set ao tempo médio de espera da lojaFE em causa
    
    public void setTempo(float tp){
    this.tempoEspera = tp;
    }
    
//metodo que devolve a classe numa string
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("Informacao FE: ").append(this.tamanhoFila)
                                    .append("\n")
                                    .append(this.tempoEspera)
                                    .append("\n");
        return sb.toString();
    }
    
    //metodo que verifica se 2 objetos são iguais
    
    public boolean equals (Object o){
       if (o == this) return true;
       if (o == null || o.getClass() != this.getClass()) return false;
       super.equals(o);
       LojaFE t = (LojaFE) o;
       return this.tamanhoFila == t.getTam() &&
              this.tempoEspera == t.getTempo();
    }
    
    
}
