package entity;

import java.util.AbstractMap;

public class Topic {
    private final AbstractMap.SimpleEntry<Integer, String> topic;

    public Topic(Integer topicId, String topicContext) {
        this.topic = new AbstractMap.SimpleEntry<>(topicId, topicContext);
    }

    public int id() {
        return topic.getKey();
    }

    public String context() {
        return topic.getValue();
    }
}
