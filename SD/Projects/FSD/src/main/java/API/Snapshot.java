package API;

import java.io.Serializable;
import java.util.*;

public class Snapshot implements Serializable {
    private final Map<Long, List<Integer>> keyVersions; // Map<Key,List<Version>>
    private final Map<Integer, Map<Integer, Boolean>> completedVersions; // Map<Peer, Map<Version, boolean>>

    public Snapshot(Map<Long, List<Integer>> keyVersions, Map<Integer, Map<Integer, Boolean>> completedVersions){
        this.keyVersions = new HashMap<>();
        keyVersions.forEach((k,v) -> this.keyVersions.put(k, new ArrayList<>(v)));
        this.completedVersions = new HashMap<>();
        completedVersions.forEach((k,v) -> this.completedVersions.put(k, new HashMap<>(v)));
    }

    public Map<Long, Integer> getVersions(List<Long> keys){
        // Most recent valid versions
        Map<Long, Integer> validVersions = new HashMap<>();

        keys.forEach(k -> {
            List<Integer> versionIDS = keyVersions.get(k);
            List<Integer> versionClone = null;
            if (versionIDS != null) versionClone = new ArrayList<>(versionIDS);
            validVersions.put(k, mostRecentValid(versionClone));
        });

        return validVersions;
    }

    private Integer mostRecentValid(List<Integer> versionIDS){
        if(versionIDS == null || versionIDS.size() == 0) return -1;
        Integer max_k = Collections.max(versionIDS);
        //System.out.println("Max is " + max_k);
        boolean allDone = true;
        for(Map<Integer, Boolean> v : completedVersions.values()){
            if(v == null || v.get(max_k) == null) return -1;
            if (!v.get(max_k)) allDone = false;
        }
        if(allDone) return max_k;
        versionIDS.remove(max_k);
        return mostRecentValid(versionIDS);
    }
}
