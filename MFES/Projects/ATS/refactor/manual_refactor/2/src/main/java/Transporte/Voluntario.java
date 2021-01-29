package Transporte;

import Perfis.Ponto2D;

/**
 * Escreva a descrição da classe Transporte.Voluntario aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Voluntario extends Transporte
{
 private boolean estado;//0-senao 1 se esta disponivelll
 public Voluntario(){
     super();
     this.estado=false;
   }
 public Voluntario(String email, String nome, String password, Ponto2D local,
                   double raio, int velmed, int licMedicamentos, boolean estado){
    super(email,nome,password,local,raio,velmed,licMedicamentos);
    this.estado=estado;
   }
 public Voluntario(Voluntario um){
     super(um);
     this.estado=um.estaDisponivel();
    }

 public boolean estaDisponivel(){return this.estado;}
 public void setDisponivel(boolean estado){ this.estado=estado;}
 
 public Voluntario clone(){return new Voluntario(this);}
} 
