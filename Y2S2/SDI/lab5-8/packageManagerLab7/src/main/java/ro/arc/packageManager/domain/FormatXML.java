package ro.arc.packageManager.domain;

import org.w3c.dom.Element;

public interface FormatXML<BaseEntity> {
    public BaseEntity fromXML(Element element);
}