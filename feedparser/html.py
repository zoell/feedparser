# Copyright 2010-2021 Kurt McKee <contactme@kurtmckee.org>
# Copyright 2002-2008 Mark Pilgrim
# All rights reserved.
#
# This file is a part of feedparser.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS'
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

import html.entities
import re

from .sgml import *

_cp1252 = {
    128: '\u20ac',  # euro sign
    130: '\u201a',  # single low-9 quotation mark
    131: '\u0192',  # latin small letter f with hook
    132: '\u201e',  # double low-9 quotation mark
    133: '\u2026',  # horizontal ellipsis
    134: '\u2020',  # dagger
    135: '\u2021',  # double dagger
    136: '\u02c6',  # modifier letter circumflex accent
    137: '\u2030',  # per mille sign
    138: '\u0160',  # latin capital letter s with caron
    139: '\u2039',  # single left-pointing angle quotation mark
    140: '\u0152',  # latin capital ligature oe
    142: '\u017d',  # latin capital letter z with caron
    145: '\u2018',  # left single quotation mark
    146: '\u2019',  # right single quotation mark
    147: '\u201c',  # left double quotation mark
    148: '\u201d',  # right double quotation mark
    149: '\u2022',  # bullet
    150: '\u2013',  # en dash
    151: '\u2014',  # em dash
    152: '\u02dc',  # small tilde
    153: '\u2122',  # trade mark sign
    154: '\u0161',  # latin small letter s with caron
    155: '\u203a',  # single right-pointing angle quotation mark
    156: '\u0153',  # latin small ligature oe
    158: '\u017e',  # latin small letter z with caron
    159: '\u0178',  # latin capital letter y with diaeresis
}


class _BaseHTMLProcessor(sgmllib.SGMLParser, object):
    special = re.compile("""[<>'"]""")
    bare_ampersand = re.compile(r"&(?!#\d+;|#x[0-9a-fA-F]+;|\w+;)")
    elements_no_end_tag = {
        'area',
        'base',
        'basefont',
        'br',
        'col',
        'command',
        'embed',
        'frame',
        'hr',
        'img',
        'input',
        'isindex',
        'keygen',
        'link',
        'meta',
        'param',
        'source',
        'track',
        'wbr',
    }

    def __init__(self, encoding=None, _type='application/xhtml+xml'):
        if encoding:
            self.encoding = encoding
        self._type = _type
        self.pieces = []
        super(_BaseHTMLProcessor, self).__init__()

    def reset(self):
        self.pieces = []
        super(_BaseHTMLProcessor, self).reset()

    def _shorttag_replace(self, match):
        """
        :type match: Match[str]
        :rtype: str
        """

        tag = match.group(1)
        if tag in self.elements_no_end_tag:
            return '<' + tag + ' />'
        else:
            return '<' + tag + '></' + tag + '>'

    # By declaring these methods and overriding their compiled code
    # with the code from sgmllib, the original code will execute in
    # feedparser's scope instead of sgmllib's. This means that the
    # `tagfind` and `charref` regular expressions will be found as
    # they're declared above, not as they're declared in sgmllib.
    def goahead(self, end):
        rawdata = self.rawdata
        i = 0
        n = len(rawdata)
        while i < n:
            if self.nomoretags:
                self.handle_data(rawdata[i:n])
                i = n
                break
            match = interesting.search(rawdata, i)
            if match: j = match.start()
            else: j = n
            if i < j:
                self.handle_data(rawdata[i:j])
            i = j
            if i == n: break
            if rawdata[i] == '<':
                if starttagopen.match(rawdata, i):
                    if self.literal:
                        self.handle_data(rawdata[i])
                        i = i+1
                        continue
                    k = self.parse_starttag(i)
                    if k < 0: break
                    i = k
                    continue
                if rawdata.startswith("</", i):
                    k = self.parse_endtag(i)
                    if k < 0: break
                    i = k
                    self.literal = 0
                    continue
                if self.literal:
                    if n > (i + 1):
                        self.handle_data("<")
                        i = i+1
                    else:
                        # incomplete
                        break
                    continue
                if rawdata.startswith("<!--", i):
                        # Strictly speaking, a comment is --.*--
                        # within a declaration tag <!...>.
                        # This should be removed,
                        # and comments handled only in parse_declaration.
                    k = self.parse_comment(i)
                    if k < 0: break
                    i = k
                    continue
                if rawdata.startswith("<?", i):
                    k = self.parse_pi(i)
                    if k < 0: break
                    i = i+k
                    continue
                if rawdata.startswith("<!", i):
                    # This is some sort of declaration; in "HTML as
                    # deployed," this should only be the document type
                    # declaration ("<!DOCTYPE html...>").
                    k = self.parse_declaration(i)
                    if k < 0: break
                    i = k
                    continue
            elif rawdata[i] == '&':
                if self.literal:
                    self.handle_data(rawdata[i])
                    i = i+1
                    continue
                match = charref.match(rawdata, i)
                if match:
                    name = match.group(1)
                    self.handle_charref(name)
                    i = match.end(0)
                    if rawdata[i-1] != ';': i = i-1
                    continue
                match = entityref.match(rawdata, i)
                if match:
                    name = match.group(1)
                    self.handle_entityref(name)
                    i = match.end(0)
                    if rawdata[i-1] != ';': i = i-1
                    continue
            else:
                self.error('neither < nor & ??')
            # We get here only if incomplete matches but
            # nothing else
            match = incomplete.match(rawdata, i)
            if not match:
                self.handle_data(rawdata[i])
                i = i+1
                continue
            j = match.end(0)
            if j == n:
                break # Really incomplete
            self.handle_data(rawdata[i:j])
            i = j
        # end while
        if end and i < n:
            self.handle_data(rawdata[i:n])
            i = n
        self.rawdata = rawdata[i:]
        # XXX if end: check for empty stack


    def __parse_starttag(self, i):
        self.__starttag_text = None
        start_pos = i
        rawdata = self.rawdata
        if shorttagopen.match(rawdata, i):
            # SGML shorthand: <tag/data/ == <tag>data</tag>
            # XXX Can data contain &... (entity or char refs)?
            # XXX Can data contain < or > (tag characters)?
            # XXX Can there be whitespace before the first /?
            match = shorttag.match(rawdata, i)
            if not match:
                return -1
            tag, data = match.group(1, 2)
            self.__starttag_text = '<%s/' % tag
            tag = tag.lower()
            k = match.end(0)
            self.finish_shorttag(tag, data)
            self.__starttag_text = rawdata[start_pos:match.end(1) + 1]
            return k
        # XXX The following should skip matching quotes (' or ")
        # As a shortcut way to exit, this isn't so bad, but shouldn't
        # be used to locate the actual end of the start tag since the
        # < or > characters may be embedded in an attribute value.
        match = endbracket.search(rawdata, i+1)
        if not match:
            return -1
        j = match.start(0)
        # Now parse the data between i+1 and j into a tag and attrs
        attrs = []
        if rawdata[i:i+2] == '<>':
            # SGML shorthand: <> == <last open tag seen>
            k = j
            tag = self.lasttag
        else:
            match = tagfind.match(rawdata, i+1)
            if not match:
                self.error('unexpected call to parse_starttag')
            k = match.end(0)
            tag = rawdata[i+1:k].lower()
            self.lasttag = tag
        while k < j:
            match = attrfind.match(rawdata, k)
            if not match: break
            attrname, rest, attrvalue = match.group(1, 2, 3)
            if not rest:
                attrvalue = attrname
            else:
                if (attrvalue[:1] == "'" == attrvalue[-1:] or
                    attrvalue[:1] == '"' == attrvalue[-1:]):
                    # strip quotes
                    attrvalue = attrvalue[1:-1]
                attrvalue = self.entity_or_charref.sub(
                    self._convert_ref, attrvalue)
            attrs.append((attrname.lower(), attrvalue))
            k = match.end(0)
        if rawdata[j] == '>':
            j = j+1
        self.__starttag_text = rawdata[start_pos:j]
        self.finish_starttag(tag, attrs)
        return j

    def parse_starttag(self, i):
        j = self.__parse_starttag(i)
        if self._type == 'application/xhtml+xml':
            if j > 2 and self.rawdata[j-2:j] == '/>':
                self.unknown_endtag(self.lasttag)
        return j

    def feed(self, data):
        """
        :type data: str
        :rtype: None
        """

        data = re.sub(r'<!((?!DOCTYPE|--|\[))', r'&lt;!\1', data, re.IGNORECASE)
        data = re.sub(r'<([^<>\s]+?)\s*/>', self._shorttag_replace, data)
        data = data.replace('&#39;', "'")
        data = data.replace('&#34;', '"')
        super(_BaseHTMLProcessor, self).feed(data)
        super(_BaseHTMLProcessor, self).close()

    @staticmethod
    def normalize_attrs(attrs):
        """
        :type attrs: List[Tuple[str, str]]
        :rtype: List[Tuple[str, str]]
        """

        if not attrs:
            return attrs
        # utility method to be called by descendants
        # Collapse any duplicate attribute names and values by converting
        # *attrs* into a dictionary, then convert it back to a list.
        attrs_d = {k.lower(): v for k, v in attrs}
        attrs = [
            (k, k in ('rel', 'type') and v.lower() or v)
            for k, v in attrs_d.items()
        ]
        attrs.sort()
        return attrs

    def unknown_starttag(self, tag, attrs):
        """
        :type tag: str
        :type attrs: List[Tuple[str, str]]
        :rtype: None
        """

        # Called for each start tag
        # attrs is a list of (attr, value) tuples
        # e.g. for <pre class='screen'>, tag='pre', attrs=[('class', 'screen')]
        uattrs = []
        strattrs = ''
        if attrs:
            for key, value in attrs:
                value = value.replace('>', '&gt;')
                value = value.replace('<', '&lt;')
                value = value.replace('"', '&quot;')
                value = self.bare_ampersand.sub("&amp;", value)
                uattrs.append((key, value))
            strattrs = ''.join(
                ' %s="%s"' % (key, value)
                for key, value in uattrs
            )
        if tag in self.elements_no_end_tag:
            self.pieces.append('<%s%s />' % (tag, strattrs))
        else:
            self.pieces.append('<%s%s>' % (tag, strattrs))

    def unknown_endtag(self, tag):
        """
        :type tag: str
        :rtype: None
        """

        # Called for each end tag, e.g. for </pre>, tag will be 'pre'
        # Reconstruct the original end tag.
        if tag not in self.elements_no_end_tag:
            self.pieces.append("</%s>" % tag)

    def handle_charref(self, ref):
        """
        :type ref: str
        :rtype: None
        """

        # Called for each character reference, e.g. '&#160;' will extract '160'
        # Reconstruct the original character reference.
        ref = ref.lower()
        if ref.startswith('x'):
            value = int(ref[1:], 16)
        else:
            value = int(ref)

        if value in _cp1252:
            self.pieces.append('&#%s;' % hex(ord(_cp1252[value]))[1:])
        else:
            self.pieces.append('&#%s;' % ref)

    def handle_entityref(self, ref):
        """
        :type ref: str
        :rtype: None
        """

        # Called for each entity reference, e.g. '&copy;' will extract 'copy'
        # Reconstruct the original entity reference.
        if ref in html.entities.name2codepoint or ref == 'apos':
            self.pieces.append('&%s;' % ref)
        else:
            self.pieces.append('&amp;%s' % ref)

    def handle_data(self, text):
        """
        :type text: str
        :rtype: None
        """

        # called for each block of plain text, i.e. outside of any tag and
        # not containing any character or entity references
        # Store the original text verbatim.
        self.pieces.append(text)

    def handle_comment(self, text):
        """
        :type text: str
        :rtype: None
        """

        # Called for HTML comments, e.g. <!-- insert Javascript code here -->
        # Reconstruct the original comment.
        self.pieces.append('<!--%s-->' % text)

    def handle_pi(self, text):
        """
        :type text: str
        :rtype: None
        """

        # Called for each processing instruction, e.g. <?instruction>
        # Reconstruct original processing instruction.
        self.pieces.append('<?%s>' % text)

    def handle_decl(self, text):
        """
        :type text: str
        :rtype: None
        """

        # called for the DOCTYPE, if present, e.g.
        # <!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
        #     "http://www.w3.org/TR/html4/loose.dtd">
        # Reconstruct original DOCTYPE
        self.pieces.append('<!%s>' % text)

    _new_declname_match = re.compile(r'[a-zA-Z][-_.a-zA-Z0-9:]*\s*').match

    def _scan_name(self, i, declstartpos):
        """
        :type i: int
        :type declstartpos: int
        :rtype: Tuple[Optional[str], int]
        """

        rawdata = self.rawdata
        n = len(rawdata)
        if i == n:
            return None, -1
        m = self._new_declname_match(rawdata, i)
        if m:
            s = m.group()
            name = s.strip()
            if (i + len(s)) == n:
                return None, -1  # end of buffer
            return name.lower(), m.end()
        else:
            self.handle_data(rawdata)
            # self.updatepos(declstartpos, i)
            return None, -1

    @staticmethod
    def convert_charref(name):
        """
        :type name: str
        :rtype: str
        """

        return '&#%s;' % name

    @staticmethod
    def convert_entityref(name):
        """
        :type name: str
        :rtype: str
        """

        return '&%s;' % name

    def output(self):
        """Return processed HTML as a single string.

        :rtype: str
        """

        return ''.join(self.pieces)

    def parse_declaration(self, i):
        """
        :type i: int
        :rtype: int
        """

        try:
            return sgmllib.SGMLParser.parse_declaration(self, i)
        except sgmllib.SGMLParseError:
            # Escape the doctype declaration and continue parsing.
            self.handle_data('&lt;')
            return i+1
