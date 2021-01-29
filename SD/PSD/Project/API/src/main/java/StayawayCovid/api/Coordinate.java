package StayawayCovid.api;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.hibernate.validator.constraints.Length;

public final class Coordinate {

	private Integer latitude;
	private Integer longitude;

	public Coordinate(Integer latitude, Integer longitude) {
		this.latitude = latitude;
		this.longitude = longitude;
	}

	public Integer getLatitude() {
		return this.latitude;
	}

	public Integer getLongitude() {
		return this.longitude;
	}
	
	@Override
	public boolean equals(Object o) {
		if (this == o ) return true;
		if (o == null || getClass() != o.getClass() ) return false;
		Coordinate coord = (Coordinate)o;
		return this.latitude==coord.getLatitude() && this.longitude==coord.getLongitude();
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(latitude);
		sb.append("x");
		sb.append(longitude);
		return sb.toString();
	}
}