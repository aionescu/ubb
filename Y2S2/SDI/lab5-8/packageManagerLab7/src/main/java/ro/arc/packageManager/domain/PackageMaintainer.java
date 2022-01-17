package ro.arc.packageManager.domain;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.io.Serializable;

import javax.persistence.Entity;

import static java.lang.Long.parseLong;

@Entity
public class PackageMaintainer extends BaseEntity<Long>  {
    Long maintainerID;
    Long packageID;

    public PackageMaintainer( Long maintainerID, Long packageID) {

        this.maintainerID = maintainerID;
        this.packageID = packageID;
    }

    public PackageMaintainer() { }

    public Long getMaintainerID() {
        return maintainerID;
    }

    public void setMaintainerID(Long maintainerID) {
        this.maintainerID = maintainerID;
    }

    public Long getPackageID() {
        return packageID;
    }

    public void setPackageID(Long packageID) {
        this.packageID = packageID;
    }

    public static class PackageMaintainerFormat implements FormatXML<PackageMaintainer>
    {
        @Override
        public PackageMaintainer fromXML(Element element) {
            String id = element.getAttribute("id");
            Node maintainerID = element.getElementsByTagName("maintainerID").item(0);
            Node packageID = element.getElementsByTagName("packageID").item(0);

            PackageMaintainer pm = new PackageMaintainer(parseLong(maintainerID.getTextContent()),parseLong(packageID.getTextContent()));
            pm.setID(parseLong(id));
            return pm;
        }
    }

    public Element toXML(Document document) {
        Element packageMaintainerElement = document.createElement("PackageMaintainer");
        String ID = getID().toString();
        packageMaintainerElement.setAttribute("id", ID);

        Element maintainerIDElement = document.createElement("maintainerID");
        maintainerIDElement.setTextContent(getMaintainerID().toString());
        packageMaintainerElement.appendChild(maintainerIDElement);

        Element packageIDElement = document.createElement("packageID");
        packageIDElement.setTextContent(getPackageID().toString());
        packageMaintainerElement.appendChild(packageIDElement);


        return packageMaintainerElement;
    }
}
