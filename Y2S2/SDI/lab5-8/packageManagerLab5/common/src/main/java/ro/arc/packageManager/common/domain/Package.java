package ro.arc.packageManager.common.domain;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import static java.lang.Long.parseLong;

public class Package extends BaseEntity<Long> {
    private String name;
    private String description;
    private String sourceRepo;
    private String license;

    public Package(Long id, String name, String description, String sourceRepo, String license) {
        super(id);

        this.name = name;
        this.description = description;
        this.sourceRepo = sourceRepo;
        this.license = license;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getSourceRepo() {
        return sourceRepo;
    }

    public void setSourceRepo(String sourceRepo) {
        this.sourceRepo = sourceRepo;
    }

    public String getLicense() {
        return license;
    }

    public void setLicense(String license) {
        this.license = license;
    }

    public static class PackageFormat implements FormatXML<Package>
    {
        @Override
        public Package fromXML(Element element) {
            String id = element.getAttribute("id");
            Node name = element.getElementsByTagName("name").item(0);
            Node description = element.getElementsByTagName("description").item(0);
            Node sourceRepo = element.getElementsByTagName("sourceRepo").item(0);
            Node license = element.getElementsByTagName("license").item(0);

            return new Package(parseLong(id),name.getTextContent(), description.getTextContent(), sourceRepo.getTextContent(),license.getTextContent());
        }
    }

    public Element toXML(Document document) {
        Element packageElement = document.createElement("Package");
        String ID = getID().toString();
        packageElement.setAttribute("id", ID);

        Element nameElement = document.createElement("name");
        nameElement.setTextContent(getName());
        packageElement.appendChild(nameElement);

        Element descriptionElement = document.createElement("description");
        descriptionElement.setTextContent(getDescription());
        packageElement.appendChild(descriptionElement);

        Element sourceRepoElement = document.createElement("sourceRepo");
        sourceRepoElement.setTextContent(getSourceRepo());
        packageElement.appendChild(sourceRepoElement);

        Element licenseElement = document.createElement("license");
        licenseElement.setTextContent(getLicense());
        packageElement.appendChild(licenseElement);

        return packageElement;
    }
}
