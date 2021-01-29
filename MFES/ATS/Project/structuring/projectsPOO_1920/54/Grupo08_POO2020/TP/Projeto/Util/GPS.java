package Projeto.Util;

import java.io.Serializable;

/**
 * Classe que implemente um Projeto.Util.GPS (coordenada).
 * Um GPS serve para localizar voluntarios, empresas, encomendas, ...
 * Esta implementaçao facilita calculos futuros, como calcular a distancia para averiguar os custos, ...
 * Consiste em dois floats, um para a longitude e outro para a latitude, estes vao ser graus entre 90 e -90.
 */
public class GPS implements Serializable {
    private float latitude;
    private float longitude;
    
    /*
     * Construtores da Classe GPS.
     * Declaracao dos construtores por omissao, parametrizado e de copia.
     */
    /**
     * Construtor por omissao de GPS.
     */
    public GPS(){
        this.latitude = 0;
        this.latitude = 0;
    }

    /**
     * Construtor parametrizado de GPS.
     * Aceita como parametros dois floats, o primeiro que sera a latitude e o segundo para a longitude.
     */
    public GPS(float lat, float longi){
        this.latitude = lat;
        this.longitude = longi;
    }

    /**
     * Construtor por copia de GPS
     * Aceita como parametro outro GPS e utiliza os metodos de acesso aos valores das variaveis de instancia
     */
    public GPS(GPS g){
        this.latitude = g.getLatitude();
        this.longitude = g.getLongitude();
    }


    /*
     * Metodos de Instancia
     */
    /**
     * Metodo que devolve o valor da latitude.
     */
    public float getLatitude(){
        return this.latitude;
    }

    /**
     * Metodo que devolve o valor da longitude.
     */
    public float getLongitude(){
        return this.longitude;
    }

    /**
     * Metodo que atualiza o valor da Latitude.
     * @param novaLatitude float com o novo valor da latitude
     */
    public void setLatitude(float novaLatitude){
        this.latitude = novaLatitude;
    }

    /**
     * Metodo que atualiza o valor da Longitude.
     * @param novaLongitude float com o novo valor da longitude
     */
    public void setLongitude(float novaLongitude){
        this.longitude = novaLongitude;
    }

    /**
     * Metodo que calcula a distancia em kms entre o GPS que recbe a mensagem e o GPS passsado como parametro.
     * Para tal usa-se a seguinte formula: 
     * d = R * arccos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon1 - lon2)).
     * @param gps uma das coordenadas para calcualr a distancia
     * @return double valor em kms obtido para a distancia
     */
    public double distancia(GPS gps) {
        int raio = 6371; //Raio da Terra em metros
        double pi = Math.PI;
        double lat1, lon1, lat2, lon2, sen1, sen2, cos1, cos2, cos12;
        //Passar as coordenadas para radianos
        lat1 = this.latitude * pi / 180;
        lon1 = this.longitude * pi / 180;
        lat2 = gps.getLatitude() * pi / 180;
        lon2 = gps.getLongitude() * pi / 180;
        //Calcular os cos e sin de lat e o cos de lon1 - lon2
        sen1 = Math.sin(lat1);
        sen2 = Math.sin(lat2);
        cos1 = Math.cos(lat1);
        cos2 = Math.cos(lat2);
        cos12 = Math.cos(lon2 - lon1);
        //Aplica se aqui a lei 
        return raio * Math.acos(sen1 * sen2 + cos1 * cos2 * cos12);
    }

    /**
     * Metodo que verifica se uma coordenada esta dentro de um circulo formado por este GPS e pelo raio recebido como parametro.
     * @param r - raio do circulo.
     * @param g - coordenda que se vai verificar se esta dentro do circulo.
     * @return Verdadeiro se o g estiver dentro do circulo.
     */
    public boolean isInsideRaio(double r, GPS g){
        System.out.println(this.distancia(g));
        return r >= this.distancia(g);
    }

    /**
     * Metodo que faz uma copia do objeto receptor da mensagem.
     * @return objeto clone do objeto que recebe a mensagem.
     */
    public GPS clone(){
        return new GPS(this);
    }

    /**
     * Metodo que determina se dois GPSs sao iguais.
     * @return boolean verdadeiro caso as coordenadas de latitude e longitude forem iguais.
     */
    public boolean equals(Object o){
        if(o == this) return true;
        if(o == null || o.getClass() != this.getClass()) return false;
        GPS g = (GPS) o;
        return this.longitude == g.getLongitude() && this.latitude == g.getLatitude();
    }

    /**
     * Metodo que devolve a representaçao em String do GPS
     * @return String com a latitude e longitude desta instancia
     */
    public String toString(){
        return "Latitude: " + this.latitude + "º" +
                "\nLongitde: " + this.longitude + "º";
    }
}