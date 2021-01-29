package State;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class MessageQueue {
    // This module implements a message queue that allows us to deal easily with the broadcasting nuances.
    private final Lock writes = new ReentrantLock();
    //private final Condition newMessages = writes.newCondition();
    private final HashMap<Integer, String> messageQueue = new HashMap<>();
    private int nextId = 0;

    public int currentID(){
        writes.lock();
        try {
            return nextId;
        } finally {
            writes.unlock();
        }
    }

    // Puts a message in the queue.
    public void putMessage(String message){
        writes.lock();
        try {
            messageQueue.put(nextId++, message);
        } finally {
            writes.unlock();
        }
    }

    // Retrieves a message from the queue.
    public String getMessage(int messageID){
        writes.lock();
        try {
            // This can never happen obviously.
            if (messageID >= nextId){
                return null;
            }
            return messageQueue.get(messageID);
        } finally {
            writes.unlock();
        }
    }
}
