package net.foggin.rules.plugin;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.TextPresentation;
import org.eclipse.jface.text.presentation.IPresentationDamager;
import org.eclipse.jface.text.presentation.IPresentationRepairer;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;


/**
 * Derived from DefaultDamagerRepairer
 */
class RuleDamagerRepairer(
    scanner : IncrementalScalaScanner,
    colorManager : ColorManager) 
    extends IPresentationDamager with IPresentationRepairer {

    /** The document this object works on */
    private var document : IDocument = null
    private var input = new EditableInput[Char]

    /*
     * @see IPresentationDamager#setDocument(IDocument)
     * @see IPresentationRepairer#setDocument(IDocument)
     */
    def setDocument(document : IDocument) {
        this.document = document;
        input = new EditableInput[Char].edit(0, 0, document.get)
    }
    
    /*
    lazy val scanner = new XMLScanner(colorManager) {
      setDefaultReturnToken(
          new Token(
            new TextAttribute(
              colorManager.getColor(ColorManager.DEFAULT))))
    }
    */
    
    /*
     * @see IPresentationDamager#getDamageRegion(ITypedRegion, DocumentEvent, boolean)
     */
    def getDamageRegion(
        partition : ITypedRegion, 
        event : DocumentEvent, 
        documentPartitioningChanged : Boolean) : IRegion = {
      
      println("Edit: ("+event.getOffset+", "+event.getLength+", "+event.getText+")")
      input = input.edit(event.getOffset, event.getLength, event.getText)
      partition
    }
        
        /*{

        if (!documentPartitioningChanged) {
            try {

                IRegion info= fDocument.getLineInformationOfOffset(e.getOffset());
                int start= Math.max(partition.getOffset(), info.getOffset());

                int end= e.getOffset() + (e.getText() == null ? e.getLength() : e.getText().length());

                if (info.getOffset() <= end && end <= info.getOffset() + info.getLength()) {
                    // optimize the case of the same line
                    end= info.getOffset() + info.getLength();
                } else
                    end= endOfLineOf(end);

                end= Math.min(partition.getOffset() + partition.getLength(), end);
                return new Region(start, end - start);

            } catch (BadLocationException x) {
            }
        }

        return partition;
    }
    */

    /*
     * @see IPresentationRepairer#createPresentation(TextPresentation, ITypedRegion)
     */
    def createPresentation(presentation : TextPresentation, region : ITypedRegion) {
      println(scanner.tokens(input).mkString("\"", "\", \"", "\""))
    }
    
    /*{
        int lastStart= region.getOffset();
        int length= 0;
        boolean firstToken= true;
        IToken lastToken= Token.UNDEFINED;
        TextAttribute lastAttribute= getTokenTextAttribute(lastToken);

        fScanner.setRange(fDocument, lastStart, region.getLength());

        while (true) {
            IToken token= fScanner.nextToken();
            if (token.isEOF())
                break;

            TextAttribute attribute= getTokenTextAttribute(token);
            if (lastAttribute != null && lastAttribute.equals(attribute)) {
                length += fScanner.getTokenLength();
                firstToken= false;
            } else {
                if (!firstToken)
                    addRange(presentation, lastStart, length, lastAttribute);
                firstToken= false;
                lastToken= token;
                lastAttribute= attribute;
                lastStart= fScanner.getTokenOffset();
                length= fScanner.getTokenLength();
            }
        }

        addRange(presentation, lastStart, length, lastAttribute);
    }*/

    /**
     * Returns a text attribute encoded in the given token. If the token's
     * data is not <code>null</code> and a text attribute it is assumed that
     * it is the encoded text attribute. It returns the default text attribute
     * if there is no encoded text attribute found.
     *
     * @param token the token whose text attribute is to be determined
     * @return the token's text attribute
     */
       /*
    protected TextAttribute getTokenTextAttribute(IToken token) {
        Object data= token.getData();
        if (data instanceof TextAttribute)
            return (TextAttribute) data;
        return null;
    }*/

    /**
     * Adds style information to the given text presentation.
     *
     * @param presentation the text presentation to be extended
     * @param offset the offset of the range to be styled
     * @param length the length of the range to be styled
     * @param attr the attribute describing the style of the range to be styled
     */
       /*
    protected void addRange(TextPresentation presentation, int offset, int length, TextAttribute attr) {
        if (attr != null) {
            int style= attr.getStyle();
            int fontStyle = style & (SWT.ITALIC | SWT.BOLD | SWT.NORMAL);
            StyleRange styleRange= new StyleRange(offset, length, attr.getForeground(), attr.getBackground(), fontStyle);
            styleRange.strikeout= (style & TextAttribute.STRIKETHROUGH) != 0;
            styleRange.underline= (style & TextAttribute.UNDERLINE) != 0;
            styleRange.font= attr.getFont();
            presentation.addStyleRange(styleRange);
        }
    }*/
}
