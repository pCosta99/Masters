package Model;

import java.io.Serializable;

public class GPS implements Serializable {

	private double x;
	private double y;

	
	public GPS() {
		this.x = 0.0;
		this.y = 0.0;
	}

	public GPS(double x, double y) {
		setX(x);
		setY(y);
	}

	public GPS(GPS c) {
		setX(c.getX());
		setY(c.getY());
	}


	public double getX() {
		return this.x;
	}

	public double getY() {
		return this.y;
	}


	public void setX(double x) {
		this.x = x;
	}

	public void setY(double y) {
		this.y = y;
	}


	public GPS clone() {
		return new GPS(this);
	}


	public boolean equals(Object o) {
		if(o == this) return true;
		if(o == null || o.getClass() != this.getClass()) return false;
		GPS g = (GPS) o;
		if(this.x == g.getX() && this.y == g.getY()) return true;
		return false;
	}


	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Coordenadas: (" + this.x + "," + this.y + ") ");
		  
		return sb.toString();
	}

}