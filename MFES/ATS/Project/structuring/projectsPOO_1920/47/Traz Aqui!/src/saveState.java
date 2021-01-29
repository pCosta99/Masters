import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import java.io.*;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

public class saveState extends Thread {
    private int testModeSelected;
    private final ReentrantLock reentrantLock = new ReentrantLock();
    public saveState(int testModeSelected){this.testModeSelected=testModeSelected;}

    public void run(trazAqui trazAqui) {
        reentrantLock.lock();
        saveObj(trazAqui);
        if(testModeSelected==1){
        saveVolunteer(trazAqui.getVolunteers());
        saveTransporter(trazAqui.getTransporters());
        saveStore(trazAqui.getStores());
        saveUser(trazAqui.getUsers());
        saveProduct(trazAqui.getProducts());
        saveOrder(trazAqui.getOrders());
        saveOrderReady(trazAqui.getOrderReady());}
        reentrantLock.unlock();
    }

    public void saveTransporter(Map<String,Transporter> transporterMap) {
        try {
            FileWriter fos = new FileWriter("estado\\Transporters.json");
            for(Transporter t : transporterMap.values()){
                JSONObject trans = new JSONObject();
                trans.put("Code",t.getCode());
                trans.put("Name",t.getName());
                trans.put("Latitude",t.getCoordinates().getLatitude());
                trans.put("Longitude",t.getCoordinates().getLongitude());
                trans.put("GeographicRadius",t.getGeographicRadius());
                trans.put("Available",t.getAvailable());
                trans.put("DeliveryType",t.getDeliveryType());
                trans.put("Speed",t.getDeliveryType());
                trans.put("Classification",t.getClassification());
                trans.put("MedicalDelivery",t.isMedicalDelivery());
                trans.put("DeliveryRegist",deliveryRegistSave(t.getDeliveryRegist()));
                trans.put("Price",t.getPrice());
                trans.put("NIF",t.getNIF());
                trans.put("deliveryCapacity",t.getDeliveryCapacity());
                fos.write(trans.toJSONString()+"\n");
            }
            fos.flush(); fos.close();
        }catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    public void saveVolunteer(Map<String,Volunteer> transporterMap) {
        try {
            FileWriter fos = new FileWriter("estado\\Volunteers.json");
            for(Volunteer vol : transporterMap.values()){
                JSONObject volunteer = new JSONObject();
                volunteer.put("Code",vol.getCode());
                volunteer.put("Name",vol.getName());
                volunteer.put("Latitude",vol.getCoordinates().getLatitude());
                volunteer.put("Longitude",vol.getCoordinates().getLongitude());
                volunteer.put("GeographicRadius",vol.getGeographicRadius());
                volunteer.put("Available",vol.getAvailable());
                volunteer.put("DeliveryType",vol.getDeliveryType());
                volunteer.put("Speed",vol.getDeliveryType());
                volunteer.put("Classification",vol.getClassification());
                volunteer.put("MedicalDelivery",vol.isMedicalDelivery());
                volunteer.put("DeliveryRegist",deliveryRegistSave(vol.getDeliveryRegist()));
                fos.write(volunteer.toJSONString()+"\n");
            }
            fos.flush(); fos.close();
        }catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    public void saveStore(Map<String,Store> stores){
        try {
            FileWriter fos = new FileWriter("estado\\Stores.json");
            for(Store st : stores.values()){
                JSONObject store = new JSONObject();
                store.put("Code",st.getCod());
                store.put("Name",st.getName());
                store.put("Latitude",st.getCoordinates().getLatitude());
                store.put("Longitude",st.getCoordinates().getLongitude());
                store.put("Queue",st.getQueue());
                store.put("AvgServiceTime",st.getAvgServiceTime());
                store.put("Products",productsCode(st.getAvailableProducts()));
                fos.write(store.toJSONString()+"\n");
            }
            fos.flush(); fos.close();
        }catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    public void saveUser(Map<String,User> user){
        try {
            FileWriter fos = new FileWriter("estado\\User.json");
            for(User us : user.values()){
                JSONObject userJSon = new JSONObject();
                userJSon.put("Code",us.getCod());
                userJSon.put("Name",us.getName());
                userJSon.put("Latitude",us.getGps().getLatitude());
                userJSon.put("Longitude",us.getGps().getLongitude());
                fos.write(userJSon.toJSONString()+"\n");
            }
            fos.flush(); fos.close();
        }catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    public void saveProduct(Map<String,Product> products){
        try {
            FileWriter fos = new FileWriter("estado\\Product.json");
            for(Product us : products.values()){
                JSONObject userJSon = new JSONObject();
                userJSon.put("Code",us.getCode());
                userJSon.put("Name",us.getName());
                userJSon.put("Weight",us.getWeight());
                userJSon.put("Price",us.getPrice());
                fos.write(userJSon.toJSONString()+"\n");
            }
            fos.flush(); fos.close();
        }catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    public void saveOrder(Map<String,Order> orders){
        try {
            FileWriter fos = new FileWriter("estado\\Order.json");
            for(Order us : orders.values()){
                JSONObject orDER = new JSONObject();
                orDER.put("Code",us.getCode());
                orDER.put("Data", us.getDate().toString());
                orDER.put("Weight",us.getWeight());
                orDER.put("Price",us.getOrderPrice());
                orDER.put("sellerCode",us.getSellerCode());
                orDER.put("buyerCode",us.getBuyerCode());
                orDER.put("deliveryCode",us.getDeliveryCode());
                orDER.put("orderType",us.getOrderType());
                orDER.put("deliveryPrice",us.getDeliveryPrice());
                orDER.put("deliveryClassification",us.getDeliveryClassification());
                orDER.put("deliveryTime",us.getOrderDeliveryTime());
                orDER.put("class",us.isClassificated());
                JSONArray linha = new JSONArray();
                linha.addAll(us.getProducts());
                orDER.put("Linha de Encomenda",linha);
                fos.write(orDER.toJSONString()+"\n");
            }
            fos.flush(); fos.close();
        }catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    public void saveOrderReady(Map<String,orderReady> orderReady){
        try {
            FileWriter fos = new FileWriter("estado\\orderReady.json");
            for(orderReady us : orderReady.values()){
                JSONObject userJSon = new JSONObject();
                userJSon.put("Code",us.getCode());
                fos.write(userJSon.toJSONString()+"\n");
            }
            fos.flush(); fos.close();
        }catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

   public JSONArray deliveryRegistSave(Map<String,Order> orders){
        JSONArray jsonArray = new JSONArray();
        jsonArray.addAll(orders.keySet());
        return jsonArray;
    }
   public JSONArray productsCode(Set<Product> productSet){
        JSONArray jsonArray = new JSONArray();
        jsonArray.addAll(productSet.stream().map(Product::getCode).collect(Collectors.toList()));
        return jsonArray;
    }

    public void saveObj(trazAqui trazAqui)  {
       try {
           ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream("trazAqui.obj"));
           out.writeObject(trazAqui);
           out.flush();
           out.close();
       }catch (Exception e){System.out.println(e.getMessage());}
    }
}
