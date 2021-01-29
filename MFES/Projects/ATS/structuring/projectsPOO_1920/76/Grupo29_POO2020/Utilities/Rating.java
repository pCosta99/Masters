package Utilities;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import Exceptions.InvalidInputException;

/**
 *   @class Rating defines a rating.
 */
public class Rating implements Serializable {
    private static final long serialVersionUID = 136L;
    private double mean;
    private Map<String, Integer> reviewers;

    /*
    *   Empty constructor creates a rating system with everything set to 0.
    */
    public Rating(){
        this.mean = 0;
        this.reviewers = new HashMap<>();
    }

    /*
    *  Simply inserts every parameter in the correct instance.
    */
    public Rating(double mean, Map<String, Integer> reviewers){
        this.mean = mean;
        this.setReviewers(reviewers);
    }

    /*
    *  Simply inserts every parameter of @param r in the correct instance.
    */
    public Rating(Rating r){
        this.mean = r.getMean();
        this.reviewers = r.getReviewers();
    }

    
    /*
    *  Sets @param mean as the mean in this Rating.
    */
    public void setMean(double mean){
        this.mean = mean;
    }
     
    /*
    *  Sets @param num as the total number of reviews in this Rating.
    */
    public void setReviewers(Map<String, Integer> reviewers){
        this.reviewers = new HashMap<>();
        reviewers.entrySet().stream().forEach(e -> this.reviewers.put(e.getKey(), e.getValue()));
    }

    /*
    *  Gets the mean in this Rating.
    */
    public double getMean(){
        return this.mean;
    }

    /*
    *  Gets the total number of reviews in this Rating.
    */
    public Map<String, Integer> getReviewers(){
        Map<String, Integer> res = new HashMap<>();
        this.reviewers.entrySet().stream().forEach(e -> res.put(e.getKey(), e.getValue()));
        return res;
    }

    /*
    *  Turns this @class Rating in a @class String. 
    */
    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append(this.mean);
        sb.append("/10");
        sb.append(" of ");
        sb.append(this.reviewers.size());

        return sb.toString();
    }

    /*
    *  Clones this @class Rating.
    */
    public Rating clone(){
        return new Rating(this.getMean(), this.getReviewers());
    }

    /*
    *  Determines if this @class Rating equals @param obj of generic class @class Object.
    */
    public boolean equals(Object obj){
        if(this == obj) return true;
        if(obj == null || this.getClass() != obj.getClass()) return false;
        Rating r = (Rating) obj;
        return r.getMean() == this.mean && r.getReviewers().equals(this.reviewers);  
    }

    public void addRate(String reviewer, int r) throws InvalidInputException{
        if(r >= 0 && r <= 10){
            this.reviewers.put(reviewer, r);
            this.mean = (this.mean*(this.reviewers.size() - 1) + r) / this.reviewers.size();
        }
        else
            throw new InvalidInputException("classificação \'" + r + "\' é inválida.");
    }


}