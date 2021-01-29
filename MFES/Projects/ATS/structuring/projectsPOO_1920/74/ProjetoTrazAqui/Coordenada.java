import java.lang.Math;
import java.io.Serializable;
public class Coordenada implements Serializable{
	private double latitude, longitude;
    
    private static int RAIOTERRA = 6371;
    /**
     * Construtor por omissão de Coordenada.
     */
	public Coordenada(){
		this.latitude = this.longitude = 0.0;
	}
  
    /**
     * Construtor parametrizado de Coordenada.
     * Aceita como parâmetros os valores da latitude e longitude.
     */
	public Coordenada(double nLatitude, double nLongitude){
		this.latitude = nLatitude;
		this.longitude = nLongitude;
	}
    
    /**
     * Construtor de cópia de Coordenada.
     * Aceita como parâmetro outra Coordenada e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */
	public Coordenada(Coordenada c){
		this.latitude = c.getLatitude();
		this.longitude = c.getLongitude();
	}

    /**
     * Actualiza o valor da latitude na coordenada.
     * 
     * @param novaLatitude novo valor da latitude
     */
	public void setLatitude(double novaLatitude){
		this.latitude = novaLatitude;
	}

    /**
     * Actualiza o valor da longitude na coordenada.
     * 
     * @param novaLongitude novo valor da longitude
     */
	public void setLongitude(double novaLongitude){
		this.longitude = novaLongitude;
	}

	/**
     * Devolve o valor da latitude da coordenada.
     * 
     * @return valor da longitude.
     */
	public double getLatitude(){
		return this.latitude;
	}
	
	/**
     * Devolve o valor da longitude da coordenada.
     * 
     * @return valor da longitude.
     */
	public double getLongitude(){
		return this.longitude;
	}

    /**
     * Método que verifica se duas Coordenadas são iguais.
     * 
     * @return boleano que indica se as duas Coordenadas são iguais ou não.
     */
	public boolean equals(Object o){
		if(this == o) return true;
		if(o == null || (this.getClass() != o.getClass())) return false;
		Coordenada c = (Coordenada) o;
		return c.getLatitude() == this.latitude && c.getLongitude() == this.longitude;
	}

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("latitude: ").append(this.latitude).append(" longitude: ").append(this.longitude).append("\n");
        return sb.toString();
    }  
    /**
     * Método que faz uma cópia do objecto receptor da mensagem.
     * Para tal invoca o construtor de cópia.
     * 
     * @return objecto clone do objecto que recebe a mensagem.
     */
	public Coordenada clone(){
		return new Coordenada(this);
	}
	public double rad(double x){return x*Math.PI/180;}
	/**
     * Método que devolve a distancia entre duas coordenada.
     * @return distancia entre duas coordenada
     */
	
    public double distancia(Coordenada o){
        return Math.sqrt(Math.pow(this.latitude-o.getLatitude(),2) + Math.pow(this.longitude-o.getLongitude(),2));
	}
}
