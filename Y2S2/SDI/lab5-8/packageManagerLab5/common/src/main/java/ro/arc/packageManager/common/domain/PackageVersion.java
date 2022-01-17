package ro.arc.packageManager.common.domain;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.io.Serializable;

import static java.lang.Long.parseLong;

public class PackageVersion extends BaseEntity<Long> {
    private Long packageID;
    private String versionNumber;

    public PackageVersion(Long id, Long packageID, String versionNumber) {
        super(id);

        this.packageID = packageID;
        this.versionNumber = versionNumber;
    }

    public Long getPackageID() {
        return packageID;
    }

    public void setPackageID(Long packageID) {
        this.packageID = packageID;
    }

    public String getVersionNumber() {
        return versionNumber;
    }

    public void setVersionNumber(String versionNumber) {
        this.versionNumber = versionNumber;
    }

    public static class PackageVersionFormat implements FormatXML<PackageVersion>
    {
        @Override
        public PackageVersion fromXML(Element element) {
            String id = element.getAttribute("id");
            Node packageID = element.getElementsByTagName("packageID").item(0);
            Node versionNumber = element.getElementsByTagName("versionNumber").item(0);
            
            return new PackageVersion(parseLong(id),parseLong(packageID.getTextContent()), versionNumber.getTextContent());
        }
    }

    public Element toXML(Document document) {
        Element packageVersionElement = document.createElement("PackageVersion");
        String ID = getID().toString();
        packageVersionElement.setAttribute("id", ID);

        Element packageIDElement = document.createElement("packageID");
        packageIDElement.setTextContent(getPackageID().toString());
        packageVersionElement.appendChild(packageIDElement);

        Element versionNumberElement = document.createElement("versionNumber");
        versionNumberElement.setTextContent(getVersionNumber());
        packageVersionElement.appendChild(versionNumberElement);

        return packageVersionElement;
    }
}