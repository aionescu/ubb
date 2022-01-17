package ro.arc.packageManager.repository;

import org.w3c.dom.*;
import org.xml.sax.SAXException;
import ro.arc.packageManager.domain.BaseEntity;
import ro.arc.packageManager.domain.FormatXML;
import ro.arc.packageManager.domain.validators.Validator;


import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.*;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.StreamSupport;

public class XMLRepository<T extends BaseEntity<Long>> implements Repository<Long, T> {
    private Path file;
    private final Validator<T> validator;
    private final FormatXML<T> formatXML;
    private long nextID = 1L;

    public XMLRepository(Validator<T> validator, String filePath, FormatXML<T> formatXML) {
        this.validator = validator;
        this.formatXML = formatXML;
        this.file = null;
        try {
            this.file = Paths.get(filePath).toAbsolutePath();
        } catch (InvalidPathException pathException) {
            pathException.printStackTrace();
        }
        var tempFileInst = new File(filePath);
        if (!tempFileInst.exists()) {
            try {
                tempFileInst.createNewFile();
            } catch (IOException err) {
                err.printStackTrace();
            }
        }
        if (tempFileInst.length() == 0) {
            try (PrintWriter writer = new PrintWriter(tempFileInst)) {
                writer.println("<repo>");
                writer.println("</repo>");
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }
        this.nextID = StreamSupport.stream(this.findAll().spliterator(), false).mapToLong(p->p.getID()).max().orElse(0L) + 1;
    }

    private NodeList getNodeList(Document document) {
        Element root = document.getDocumentElement();
        return root.getChildNodes();
    }

    private Document getDocument() {
        DocumentBuilder documentBuilder;
        try {
            documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            Document doc;
            doc = documentBuilder.parse(this.file.toFile());
            return doc;
        } catch (IOException | SAXException | ParserConfigurationException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public Optional<T> findOne(Long id) {
        Optional.ofNullable(id).orElseThrow(() -> new IllegalArgumentException("ID is null"));
        NodeList elements = this.getNodeList(this.getDocument());
        return IntStream.range(0, elements.getLength())
                .mapToObj(elements::item)
                .filter(node -> node instanceof Element)
                .map(node -> formatXML.fromXML((Element) node))
                .filter(el -> id.equals(el.getID()))
                .findFirst();
    }

    @Override
    public Iterable<T> findAll() {
        NodeList elements = this.getNodeList(this.getDocument());
        return IntStream.range(0, elements.getLength())
                .mapToObj(elements::item)
                .filter(node -> node instanceof Element)
                .map(node -> formatXML.fromXML((Element) node))
                .collect(Collectors.toList());
    }

    public static void removeWhitespaces(Element element) {
        NodeList children = element.getChildNodes();
        for (int i = children.getLength() - 1; i >= 0; i--) {
            Node child = children.item(i);
            if (child instanceof Text
                    && ((Text) child).getData().trim().isEmpty()) {
                element.removeChild(child);
            } else if (child instanceof Element) {
                removeWhitespaces((Element) child);
            }
        }
    }

    @Override
    public Optional<T> save(T entity) {
        Optional.ofNullable(entity).orElseThrow(() -> new IllegalArgumentException("Entity is null"));
        entity.setID(this.nextID);
        validator.validate(entity);
        Document document = this.getDocument();
        NodeList elements = this.getNodeList(this.getDocument());
        Element root = document.getDocumentElement();
        Optional<T> foundElement = IntStream.range(0, elements.getLength())
                .mapToObj(elements::item)
                .filter(node -> node instanceof Element)
                .map(node -> formatXML.fromXML((Element) node))
                .filter(tElem -> entity.getID().equals(tElem.getID()))
                .findFirst();
        foundElement.ifPresentOrElse(
                obj -> {
                },
                () -> {
                    document.getDocumentElement().appendChild(entity.toXML(document));
                    removeWhitespaces(root);
                    this.saveToFile(document);
                    this.nextID++;
                }
        );
        return foundElement;
    }

    private void saveToFile(Document document) {
        Transformer transformer;
        try {
            transformer = TransformerFactory.newInstance().newTransformer();
            DOMSource source = new DOMSource(document);
            FileWriter writer = new FileWriter(this.file.toFile());
            StreamResult result = new StreamResult(writer);
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            transformer.transform(source, result);
        } catch (IOException | TransformerException e) {
            e.printStackTrace();
        }
    }

    @Override
    public Optional<T> delete(Long id) {
        Optional.ofNullable(id).orElseThrow(() -> new IllegalArgumentException("ID is null"));
        Document document = this.getDocument();
        NodeList elements = this.getNodeList(document);
        Element root = document.getDocumentElement();
        final Optional<T>[] found = new Optional[]{Optional.empty()};
        IntStream.range(0, elements.getLength())
                .mapToObj(elements::item)
                .filter(node -> node instanceof Element)
                .filter(elem -> id.equals((formatXML.fromXML((Element) elem)).getID()))
                .findFirst()
                .ifPresentOrElse(
                        node -> {
                            T elem = formatXML.fromXML((Element) node);
                            found[0] = Optional.of(elem);
                            root.removeChild(node);
                        },
                        () -> {
                        }
                );
        removeWhitespaces(root);
        this.saveToFile(document);
        return found[0];
    }

    @Override
    public Optional<T> update(T entity) {
        Optional.ofNullable(entity).orElseThrow(() -> new IllegalArgumentException("Entity is null"));
        validator.validate(entity);
        Document document = this.getDocument();
        NodeList elements = this.getNodeList(document);
        Element root = document.getDocumentElement();
        final Optional<T>[] found = new Optional[]{Optional.of(entity)};
        try {
            IntStream.range(0, elements.getLength())
                    .mapToObj(elements::item)
                    .filter(node -> node instanceof Element)
                    .filter(elem -> entity.getID().equals(((formatXML.fromXML((Element) elem))).getID()))
                    .findFirst()
                    .ifPresentOrElse(
                            node -> {
                                found[0] = Optional.empty();
                                root.removeChild(node);
                                root.appendChild(entity.toXML(document));
                            },
                            () -> {
                            }
                    );
        } catch (RuntimeException e) {
            e.printStackTrace();
        }
        removeWhitespaces(root);
        this.saveToFile(document);
        return found[0];
    }
}