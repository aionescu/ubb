package ro.arc.packageManager.common.domain;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import static java.lang.Long.parseLong;

public class PackageMaintainer extends BaseEntity<Long> {
    Long maintainerID;
    Long packageID;

    public PackageMaintainer(Long id, Long maintainerID, Long packageID) {
        super(id);

        this.maintainerID = maintainerID;
        this.packageID = packageID;
    }

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

            return new PackageMaintainer(parseLong(id),parseLong(maintainerID.getTextContent()),parseLong(packageID.getTextContent()));
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
