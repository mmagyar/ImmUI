//package mmagyar.parsing
//
//import java.io._
//import java.util.StringTokenizer
//import java.util.regex.Matcher
//import java.util.regex.Pattern
//
//class BitmapFontData()
//
///** Creates an empty BitmapFontData for configuration before calling {@link #load(FileHandle, boolean)}, to subclass, or to
//  * populate yourself, e.g. using stb-truetype or FreeType. */
//{
//
//
//  /** An array of the image paths, for multiple texture pages. */
//  var imagePaths:Array[String] = null
//  var fontFile:File = null
//  var flipped = false
//  var padTop = .0
//  var padRight = .0
//  var padBottom = .0
//  var padLeft = .0
//  /** The distance from one line of text to the next. To set this value, use {@link #setLineHeight(float)}. */
//  var lineHeight = .0
//  /** The distance from the top of most uppercase characters to the baseline. Since the drawing position is the cap height of
//    * the first line, the cap height can be used to get the location of the baseline. */
//  var capHeight = 1
//  /** The distance from the cap height to the top of the tallest glyph. */
//  var ascent = .0
//  /** The distance from the bottom of the glyph that extends the lowest to the baseline. This number is negative. */
//  var descent = .0
//  /** The distance to move down when \n is encountered. */
//  var down = .0
//  /** Multiplier for the line height of blank lines. down * blankLineHeight is used as the distance to move down for a blank
//    * line. */
//  var blankLineScale = 1
//  var scaleX = 1
//  var scaleY = 1
//  var markupEnabled = false
//  /** The amount to add to the glyph X position when drawing a cursor between glyphs. This field is not set by the BMFont
//    * file, it needs to be set manually depending on how the glyphs are rendered on the backing textures. */
//  var cursorX = .0
//  final val glyphs = new Array[Array[BitmapFont.Glyph]](BitmapFont.PAGES)
//  /** The glyph to display for characters not in the font. May be null. */
//  var missingGlyph:BitmapFont.Glyph = null
//  /** The width of the space character. */
//  var spaceWidth = .0
//  /** The x-height, which is the distance from the top of most lowercase characters to the baseline. */
//  var xHeight = 1
//  /** Additional characters besides whitespace where text is wrapped. Eg, a hypen (-). */
//  var breakChars = null
//  var xChars = Array('x', 'e', 'a', 'o', 'n', 's', 'r', 'c', 'u', 'm', 'v', 'w', 'z')
//  var capChars = Array('M', 'N', 'B', 'D', 'C', 'E', 'F', 'K', 'A', 'G', 'H', 'I', 'J', 'L', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
//
//  def this(fontFile: File, flip: Boolean) {
//    this()
//    this.fontFile = fontFile
//    this.flipped = flip
//    load(fontFile, flip)
//  }
//
//  def load(fontFile: File, flip: Boolean) {
//    if (imagePaths != null) throw new IllegalStateException("Already loaded.")
//    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(fontFile)), 512)
//    try
//      var line = reader.readLine // info
//      if (line == null) throw new Nothing("File is empty.")
//      line = line.substring(line.indexOf("padding=") + 8)
//      val padding = line.substring(0, line.indexOf(' ')).split(",", 4)
//      if (padding.length != 4) throw new Nothing("Invalid padding.")
//      padTop = padding(0).toInt
//      padRight = padding(1).toInt
//      padBottom = padding(2).toInt
//      padLeft = padding(3).toInt
//      val padY = padTop + padBottom
//      line = reader.readLine
//      if (line == null) throw new Nothing("Missing common header.")
//      val common = line.split(" ", 7) // At most we want the 6th element; i.e. "page=N"
//      // At least lineHeight and base are required.
//      if (common.length < 3) throw new Nothing("Invalid common header.")
//      if (!common(1).startsWith("lineHeight=")) throw new Nothing("Missing: lineHeight")
//      lineHeight = common(1).substring(11).toInt
//      if (!common(2).startsWith("base=")) throw new Nothing("Missing: base")
//      val baseLine = common(2).substring(5).toInt
//      var pageCount = 1
//      if (common.length >= 6 && common(5) != null && common(5).startsWith("pages=")) try
//        pageCount = Math.max(1, common(5).substring(6).toInt)
//
//      catch {
//        case ignored: NumberFormatException => {
//          // Use one page.
//        }
//      }
//      imagePaths = new Array[String](pageCount)
//      // Read each page definition.
//      var p = 0
//      while (p < pageCount) {
//        {
//          // Read each "page" info line.
//          line = reader.readLine
//          if (line == null) throw new Nothing("Missing additional page definitions.")
//          // Expect ID to mean "index".
//          var matcher = Pattern.compile(".*id=(\\d+)").matcher(line)
//          if (matcher.find) {
//            val id = matcher.group(1)
//            try
//              val pageID = id.toInt
//              if (pageID != p) throw new Nothing("Page IDs must be indices starting at 0: " + id)
//
//            catch {
//              case ex: NumberFormatException => {
//                throw new Nothing("Invalid page id: " + id, ex)
//              }
//            }
//          }
//          matcher = Pattern.compile(".*file=\"?([^\"]+)\"?").matcher(line)
//          if (!matcher.find) throw new Nothing("Missing: file")
//          val fileName = matcher.group(1)
//          imagePaths(p) = fontFile.getParentFile.listFiles().find(_.getName == fileName).get.getAbsolutePath.replaceAll("\\\\", "/")
//        }
//        {
//          p += 1;
//          p - 1
//        }
//      }
//      descent = 0
//
//      def parsing():Unit =    {
//        line = reader.readLine
//        if (line == null) () //todo: break is not supported// EOF
//        else if (line.startsWith("kernings ")) () //todo: break is not supported// Starting kernings block.
//        else if (!line.startsWith("char ")) parsing()
//        else {
//        val glyph = new BitmapFont.Glyph
//        val tokens = new StringTokenizer(line, " =")
//        tokens.nextToken
//        tokens.nextToken
//        val ch = tokens.nextToken.toInt
//        val continue = if (ch <= 0) {missingGlyph = glyph
//        true}
//        else if (ch <= Character.MAX_VALUE) {setGlyph(ch, glyph)
//        true}
//        else false
//          if(!continue) parsing()
//          else {
//        glyph.id = ch
//        tokens.nextToken
//        glyph.srcX = tokens.nextToken.toInt
//        tokens.nextToken
//        glyph.srcY = tokens.nextToken.toInt
//        tokens.nextToken
//        glyph.width = tokens.nextToken.toInt
//        tokens.nextToken
//        glyph.height = tokens.nextToken.toInt
//        tokens.nextToken
//        glyph.xoffset = tokens.nextToken.toInt
//        tokens.nextToken
//        if (flip) glyph.yoffset = tokens.nextToken.toInt
//        else glyph.yoffset = -(glyph.height + tokens.nextToken.toInt)
//        tokens.nextToken
//        glyph.xadvance = tokens.nextToken.toInt
//        // Check for page safely, it could be omitted or invalid.
//        if (tokens.hasMoreTokens) tokens.nextToken
//        if (tokens.hasMoreTokens)
//          try glyph.page = tokens.nextToken.toInt
//        catch {case ignored: NumberFormatException => ()}
//        if (glyph.width > 0 && glyph.height > 0) descent = Math.min(baseLine + glyph.yoffset, descent)
//      }}}
//
//      descent += padBottom
//
//      def parsing2():Unit = {
//      while (true) {
//        line = reader.readLine
//        if (line == null) ()
//        else if (!line.startsWith("kerning ")) ()
//        else {
//        //todo: break is not supported
//        val tokens = new StringTokenizer(line, " =")
//        tokens.nextToken
//        tokens.nextToken
//        val first = tokens.nextToken.toInt
//        tokens.nextToken
//        val second = tokens.nextToken.toInt
//        if (first < 0 || first > Character.MAX_VALUE || second < 0 || second > Character.MAX_VALUE) parsing2()
//        else {
//        //todo: continue is not supported
//        val glyph = getGlyph(first.toChar)
//        tokens.nextToken
//        val amount = tokens.nextToken.toInt
//        if (glyph != null) {
//          // Kernings may exist for glyph pairs not contained in the font.
//          glyph.setKerning(second, amount)
//        }}}
//      }
//      }
//      var spaceGlyph = getGlyph(' ')
//      if (spaceGlyph == null) {
//        spaceGlyph = new BitmapFont.Glyph
//        spaceGlyph.id = ' '.toInt
//        var xadvanceGlyph = getGlyph('l')
//        if (xadvanceGlyph == null) xadvanceGlyph = getFirstGlyph
//        spaceGlyph.xadvance = xadvanceGlyph.xadvance
//        setGlyph(' ', spaceGlyph)
//      }
//      if (spaceGlyph.width == 0) {
//        spaceGlyph.width = (padLeft + spaceGlyph.xadvance + padRight).toInt
//        spaceGlyph.xoffset = -padLeft.toInt
//      }
//      spaceWidth = spaceGlyph.width
//      var xGlyph = null
//      for (xChar <- xChars) {
//        xGlyph = getGlyph(xChar)
//        if (xGlyph != null) break //todo: break is not supported
//      }
//      if (xGlyph == null) xGlyph = getFirstGlyph
//      xHeight = xGlyph.height - padY
//      var capGlyph = null
//      for (capChar <- capChars) {
//        capGlyph = getGlyph(capChar)
//        if (capGlyph != null) break //todo: break is not supported
//      }
//      if (capGlyph == null) for (page <- this.glyphs) {
//        if (page == null) continue //todo: continue is not supported
//        for (glyph <- page) {
//          if (glyph == null || glyph.height == 0 || glyph.width == 0) continue //todo: continue is not supported
//          capHeight = Math.max(capHeight, glyph.height)
//        }
//      }
//      else capHeight = capGlyph.height
//      capHeight -= padY
//      ascent = baseLine - capHeight
//      down = -lineHeight
//      if (flip) {
//        ascent = -ascent
//        down = -down
//      }
//
//    catch {
//      case ex: Exception => {
//        throw new Nothing("Error loading font file: " + fontFile, ex)
//      }
//    } finally StreamUtils.closeQuietly(reader)
//  }
//
//  def setGlyphRegion(glyph: BitmapFont.Glyph, region: Nothing) {
//    val texture = region.getTexture
//    val invTexWidth = 1.0f / texture.getWidth
//    val invTexHeight = 1.0f / texture.getHeight
//    var offsetX = 0
//    var offsetY = 0
//    val u = region.u
//    val v = region.v
//    val regionWidth = region.getRegionWidth
//    val regionHeight = region.getRegionHeight
//    if (region.isInstanceOf[Nothing]) {
//      // Compensate for whitespace stripped from left and top edges.
//      val atlasRegion = region.asInstanceOf[Nothing]
//      offsetX = atlasRegion.offsetX
//      offsetY = atlasRegion.originalHeight - atlasRegion.packedHeight - atlasRegion.offsetY
//    }
//    var x = glyph.srcX
//    var x2 = glyph.srcX + glyph.width
//    var y = glyph.srcY
//    var y2 = glyph.srcY + glyph.height
//    // Shift glyph for left and top edge stripped whitespace. Clip glyph for right and bottom edge stripped whitespace.
//    if (offsetX > 0) {
//      x -= offsetX
//      if (x < 0) {
//        glyph.width += x
//        glyph.xoffset -= x
//        x = 0
//      }
//      x2 -= offsetX
//      if (x2 > regionWidth) {
//        glyph.width -= x2 - regionWidth
//        x2 = regionWidth
//      }
//    }
//    if (offsetY > 0) {
//      y -= offsetY
//      if (y < 0) {
//        glyph.height += y
//        y = 0
//      }
//      y2 -= offsetY
//      if (y2 > regionHeight) {
//        val amount = y2 - regionHeight
//        glyph.height -= amount
//        glyph.yoffset += amount
//        y2 = regionHeight
//      }
//    }
//    glyph.u = u + x * invTexWidth
//    glyph.u2 = u + x2 * invTexWidth
//    if (flipped) {
//      glyph.v = v + y * invTexHeight
//      glyph.v2 = v + y2 * invTexHeight
//    }
//    else {
//      glyph.v2 = v + y * invTexHeight
//      glyph.v = v + y2 * invTexHeight
//    }
//  }
//
//  /** Sets the line height, which is the distance from one line of text to the next. */
//  def setLineHeight(height: Float) {
//    lineHeight = height * scaleY
//    down = if (flipped) lineHeight
//    else -lineHeight
//  }
//
//  def setGlyph(ch: Int, glyph: BitmapFont.Glyph) {
//    var page = glyphs(ch / PAGE_SIZE)
//    if (page == null) glyphs(ch / PAGE_SIZE) = page = new Array[BitmapFont.Glyph](PAGE_SIZE)
//    page(ch & PAGE_SIZE - 1) = glyph
//  }
//
//  def getFirstGlyph: BitmapFont.Glyph = {
//    for (page <- this.glyphs) {
//      if (page == null) continue //todo: continue is not supported
//      for (glyph <- page) {
//        if (glyph == null || glyph.height == 0 || glyph.width == 0) continue //todo: continue is not supported
//        return glyph
//      }
//    }
//    throw new Nothing("No glyphs found.")
//  }
//
//  /** Returns true if the font has the glyph, or if the font has a {@link #missingGlyph}. */
//  def hasGlyph(ch: Char): Boolean = {
//    if (missingGlyph != null) return true
//    getGlyph(ch) != null
//  }
//
//  /** Returns the glyph for the specified character, or null if no such glyph exists. Note that
//    * {@link #getGlyphs(GlyphRun, CharSequence, int, int, boolean)} should be be used to shape a string of characters into a
//    * list of glyphs. */
//  def getGlyph(ch: Char): BitmapFont.Glyph = {
//    val page = glyphs(ch / PAGE_SIZE)
//    if (page != null) return page(ch & PAGE_SIZE - 1)
//    null
//  }
//
//  /** Using the specified string, populates the glyphs and positions of the specified glyph run.
//    *
//    * @param str         Characters to convert to glyphs. Will not contain newline or color tags. May contain "[[" for an escaped left
//    *                    square bracket.
//    * @param tightBounds If true, the first { @link GlyphRun#xAdvances} entry is offset to prevent the first glyph from being
//    *                                               drawn left of 0 and the last entry is offset to prevent the last glyph from being drawn right of the run
//    *           width. */
//  def getGlyphs(run: Nothing, str: CharSequence, start: Int, end: Int, tightBounds: Boolean) {
//    val markupEnabled = this.markupEnabled
//    val scaleX = this.scaleX
//    val missingGlyph = this.missingGlyph
//    val glyphs = run.glyphs
//    val xAdvances = run.xAdvances
//    // Guess at number of glyphs needed.
//    glyphs.ensureCapacity(end - start)
//    xAdvances.ensureCapacity(end - start + 1)
//    var lastGlyph = null
//    while (start < end) {
//      val ch = str.charAt({
//        start += 1; start - 1
//      })
//      var glyph = getGlyph(ch)
//      if (glyph == null) {
//        if (missingGlyph == null) continue //todo: continue is not supported
//        glyph = missingGlyph
//      }
//      glyphs.add(glyph)
//      if (lastGlyph == null) // First glyph.
//        xAdvances.add(if (!tightBounds || glyph.fixedWidth) 0
//        else -glyph.xoffset * scaleX - padLeft)
//      else xAdvances.add((lastGlyph.xadvance + lastGlyph.getKerning(ch)) * scaleX)
//      lastGlyph = glyph
//      // "[[" is an escaped left square bracket, skip second character.
//      if (markupEnabled && ch == '[' && start < end && str.charAt(start) == '[') {
//        start += 1; start - 1
//      }
//    }
//    if (lastGlyph != null) {
//      val lastGlyphWidth = if (!tightBounds || lastGlyph.fixedWidth) lastGlyph.xadvance
//      else lastGlyph.xoffset + lastGlyph.width - padRight
//      xAdvances.add(lastGlyphWidth * scaleX)
//    }
//  }
//
//  /** Returns the first valid glyph index to use to wrap to the next line, starting at the specified start index and
//    * (typically) moving toward the beginning of the glyphs array. */
//  def getWrapIndex(glyphs: Nothing, start: Int): Int = {
//    var i = start - 1
//    while (i >= 1) if (!isWhitespace(glyphs.get(i).id.asInstanceOf[Char])) break //todo: break is not supported
//    {
//      i -= 1; i + 1
//    }
//    while (i >= 1) {
//      {
//        val ch = glyphs.get(i).id.asInstanceOf[Char]
//        if (isWhitespace(ch) || isBreakChar(ch)) return i + 1
//      }
//      {
//        i -= 1; i + 1
//      }
//    }
//    0
//  }
//
//  def isBreakChar(c: Char): Boolean = {
//    if (breakChars == null) return false
//    for (br <- breakChars) if (c == br) return true
//    false
//  }
//
//  def isWhitespace(c: Char): Boolean = c match {
//    case '\n' =>
//    case '\r' =>
//    case '\t' =>
//    case ' ' =>
//      true
//    case _ =>
//      false
//  }
//
//  /** Returns the image path for the texture page at the given index (the "id" in the BMFont file). */
//  def getImagePath(index: Int): String = imagePaths(index)
//
//  def getImagePaths: Array[String] = imagePaths
//
//  def getFontFile: Nothing = fontFile
//
//  /** Scales the font by the specified amounts on both axes
//    * <p>
//    * Note that smoother scaling can be achieved if the texture backing the BitmapFont is using {@link TextureFilter#Linear}.
//    * The default is Nearest, so use a BitmapFont constructor that takes a {@link TextureRegion}.
//    *
//    * @throws IllegalArgumentException if scaleX or scaleY is zero. */
//  def setScale(scaleX: Float, scaleY: Float) {
//    if (scaleX == 0) throw new IllegalArgumentException("scaleX cannot be 0.")
//    if (scaleY == 0) throw new IllegalArgumentException("scaleY cannot be 0.")
//    val x = scaleX / this.scaleX
//    val y = scaleY / this.scaleY
//    lineHeight *= y
//    spaceWidth *= x
//    xHeight *= y
//    capHeight *= y
//    ascent *= y
//    descent *= y
//    down *= y
//    padTop *= y
//    padLeft *= y
//    padBottom *= y
//    padRight *= y
//    this.scaleX = scaleX
//    this.scaleY = scaleY
//  }
//
//  /** Scales the font by the specified amount in both directions.
//    *
//    * @see #setScale(float, float)
//    * @throws IllegalArgumentException if scaleX or scaleY is zero. */
//  def setScale(scaleXY: Float) {
//    setScale(scaleXY, scaleXY)
//  }
//
//  /** Sets the font's scale relative to the current scale.
//    *
//    * @see #setScale(float, float)
//    * @throws IllegalArgumentException if the resulting scale is zero. */
//  def scale(amount: Float) {
//    setScale(scaleX + amount, scaleY + amount)
//  }
//}
//object BitmapFont {
//   val LOG2_PAGE_SIZE = 9
//   val PAGE_SIZE      = 1 << LOG2_PAGE_SIZE
//   val PAGES          = 0x10000 / PAGE_SIZE
//
//  def indexOf(text: CharSequence, ch: Char, start: Int) :Int= {
//    val n = text.length
//    var i = start
//    while (i < n) {if (text.charAt(i) == ch) return i
//      i += 1
//    }
//    n
//  }
//
//  class Glyph {
//    var id                                  = 0
//    var srcX                                = 0
//    var srcY                                = 0
//    var width                               = 0
//    var height                              = 0
//    var u                                   = .0
//    var v                                   = .0
//    var u2                                  = .0
//    var v2                                  = .0
//    var xoffset                             = 0
//    var yoffset                             = 0
//    var xadvance                            = 0
//    var kerning: Option[Array[Array[Byte]]] = None
//    var fixedWidth                          = false
//
//    /** The index to the texture page that holds this glyph. */
//    var page = 0
//
//    def getKerning(ch: Char): Int = {
//      //todo kerning
//      //      if (kerning != null) {
//      //        val page = kerning(ch >>> LOG2_PAGE_SIZE)
//      //        if (page != null) return page(ch & PAGE_SIZE - 1)
//      //      }
//      0
//    }
//
//    def setKerning(ch: Int, value: Int) {
//      //TODO kerning
//      //if (kerning == null) kerning = new Array[Array[Byte]](PAGES)
//      //var page = kerning(ch >>> LOG2_PAGE_SIZE)
//      //if (page == null) kerning(ch >>> LOG2_PAGE_SIZE) = page = new Array[Byte](PAGE_SIZE)
//      //page(ch & PAGE_SIZE - 1) = value.toByte
//    }
//
//    override def toString: String = Character.toString(id.toChar)
//  }
//
//}
//
//
//
//class BitmapFont {
//  protected def load(data: BitmapFontData) {
//    for (page <- data.glyphs) {
//      if (page == null) continue //todo: continue is not supported
//      for (glyph <- page) if (glyph != null) data.setGlyphRegion(glyph, regions.get(glyph.page))
//    }
//    if (data.missingGlyph != null)
//      data.setGlyphRegion(data.missingGlyph, regions.get(data.missingGlyph.page))
//  }
//}
