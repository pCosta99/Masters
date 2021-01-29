package State;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Clients {
    private List<Client> clients;
    private Lock clientsLock;

    public Clients(){
        clients = new ArrayList<Client>();
        clientsLock = new ReentrantLock();
    }

    public void addClient(Client c){
        clientsLock.lock();
        try {
            this.clients.add(c);
        } finally {
            clientsLock.unlock();
        }
    }

    public void removeClient(Client c){
        clientsLock.lock();
        try {
            this.clients.remove(c);
        } finally {
            clientsLock.unlock();
        }
    }

    public List<Client> getClients(){
        clientsLock.lock();
        try {
            return this.clients;
        } finally {
            clientsLock.unlock();
        }
    }
}
