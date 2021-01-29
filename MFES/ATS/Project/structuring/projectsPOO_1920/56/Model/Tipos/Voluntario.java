package Model.Tipos;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * Classe que representa um voluntário
 */

public class Voluntario extends Tipo implements IVoluntario, Serializable {

    private float radius_volunteer;
    private boolean availability;
    private boolean medicamentos;

    private int volunteer_rating;


    /**
     * Construtor por omissão
     */
    public Voluntario(){
        super();
        this.radius_volunteer = 0;
        this.availability = true;
        this.medicamentos = false;
        this.volunteer_rating = 0;

    }

    /**
     * Construtor por cópia
     */
    public Voluntario(Voluntario voluntario){

        super(voluntario);
        this.radius_volunteer= voluntario.getRadius_volunteer();
        this.availability= voluntario.getAvailability();
        this.volunteer_rating= voluntario.getVolunteer_rating();
    }

    /**
     * Construtor por parametro
     */
    public Voluntario(String id_volunteer,String name, String id_package, String id_store_pickup, List<String> package_list,
                      float x_volunteer, float y_volunteer,
                      float radius_volunteer, boolean availability, int volunteer_rating, LocalDateTime time_start,
                      LocalDateTime time_finish){

        super(id_volunteer,name,x_volunteer,y_volunteer);
        this.radius_volunteer=radius_volunteer;
        this.availability=availability;
        this.volunteer_rating=volunteer_rating;

    }

    public boolean getMedicamentos() {
        return medicamentos;
    }

    public void setMedicamentos(boolean medicamentos) {
        this.medicamentos = medicamentos;
    }

    public float getRadius_volunteer() {
        return this.radius_volunteer;
    }
    public void setRadius_volunteer(float radius_volunteer) {
        this.radius_volunteer = radius_volunteer;
    }


    public boolean getAvailability(){
        return this.availability;
    }
    public void setAvailability(boolean availability){
        this.availability=availability;
    }
    public int getVolunteer_rating(){
        return this.volunteer_rating;
    }
    public void setVolunteer_rating(int volunteer_rating){
        this.volunteer_rating=volunteer_rating;
    }



    public Voluntario clone(){
        return new Voluntario(this);
    }

    public boolean equals(Object obj) {
        if (obj==this) return true;
        if (obj==null || obj.getClass()!=this.getClass()) return false;
        Voluntario v = (Voluntario) obj;
        return this.radius_volunteer==v.getRadius_volunteer() &&
                this.availability==(v.getAvailability()) && this.volunteer_rating==v.getVolunteer_rating();
    }

    public String toString() {
        return "\nVoluntario:\n" +
                this.getId() + "\n" +
                this.getNome() + "\n" +
                this.getX() + "\n" +
                this.getY() + "\n" +
                radius_volunteer ;
    }

}
