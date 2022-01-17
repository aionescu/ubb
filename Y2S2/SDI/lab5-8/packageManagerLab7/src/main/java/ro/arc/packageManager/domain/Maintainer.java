package ro.arc.packageManager.domain;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import javax.persistence.Column;
import javax.persistence.Entity;
import java.io.Serializable;

import static java.lang.Long.parseLong;

@Entity
public class Maintainer extends BaseEntity<Long> {
    private String userName;
    private String fullName;
    private String email;

    public Maintainer( String userName, String fullName, String email) {

        this.userName = userName;
        this.fullName = fullName;
        this.email = email;
    }

    public Maintainer() {
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getFullName() {
        return fullName;
    }

    public void setFullName(String fullName) {
        this.fullName = fullName;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public static class MaintainerFormat implements FormatXML<Maintainer>
    {
        @Override
        public Maintainer fromXML(Element element) {
            String id = element.getAttribute("id");
            Node userName = element.getElementsByTagName("userName").item(0);
            Node fullName = element.getElementsByTagName("fullName").item(0);
            Node email = element.getElementsByTagName("email").item(0);

            Maintainer m = new Maintainer(userName.getTextContent(), fullName.getTextContent(), email.getTextContent());
            m.setID(parseLong(id));
            return m;
        }
    }

    public Element toXML(Document document) {
        Element maintainerElement = document.createElement("Maintainer");
        String ID = getID().toString();
        maintainerElement.setAttribute("id", ID);

        Element userNameElement = document.createElement("userName");
        userNameElement.setTextContent(getUserName());
        maintainerElement.appendChild(userNameElement);

        Element fullNameElement = document.createElement("fullName");
        fullNameElement.setTextContent(getFullName());
        maintainerElement.appendChild(fullNameElement);

        Element emailElement = document.createElement("email");
        emailElement.setTextContent(getEmail());
        maintainerElement.appendChild(emailElement);

        return maintainerElement;
    }
}