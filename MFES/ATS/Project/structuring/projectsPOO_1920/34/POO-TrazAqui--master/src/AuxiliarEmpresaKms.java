import java.util.Comparator;
/**
	* Comparator de AuxiliarEmpresaKms.
	*/
	class AuxiliarEmpresaKmsComparator implements Comparator<AuxiliarEmpresaKms>{
 
	    @Override
    	public int compare(AuxiliarEmpresaKms e1, AuxiliarEmpresaKms e2) {
        	return -1* Double.compare(e1.getKmsPercorridos(), e2.getKmsPercorridos());
	} 
}

public class AuxiliarEmpresaKms {


	String codTransportador;
	double kmsPercorridos;

	public AuxiliarEmpresaKms() {
		this.codTransportador = "";
		this.kmsPercorridos = 0;
	}

	public AuxiliarEmpresaKms(String codTransportador, double kmsPercorridos){
		this.codTransportador = codTransportador;
		this.kmsPercorridos = kmsPercorridos;
	}

	public AuxiliarEmpresaKms(AuxiliarEmpresaKms a){
		this.codTransportador = a.getCodTransportador();
		this.kmsPercorridos  = a.getKmsPercorridos();
	}

	public String getCodTransportador(){
		return this.codTransportador;
	}

	public double getKmsPercorridos(){
		return this.kmsPercorridos;
	}

	public void setCodTransportador(String codTransportador){
		this.codTransportador = codTransportador;
		this.kmsPercorridos = kmsPercorridos;
	}

	public void setKmsPercorridos(double kmsPercorridos){
		this.kmsPercorridos = kmsPercorridos;
	}

	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append(this.codTransportador + "\n");
		sb.append(this.kmsPercorridos + "\n");
	
		return sb.toString();
	}

	public AuxiliarEmpresaKms clone(){
		return new AuxiliarEmpresaKms(this);
	}
}