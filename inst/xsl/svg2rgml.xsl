<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:svg="http://www.w3.org/2000/svg"
                xmlns:rgml="http://r-project.org/RGML"
                version='1.0'>

  <xsl:output method="xml" indent="yes" />

  <xsl:template match="/">
    <rgml:picture version="3">
      <xsl:apply-templates />
      <!-- Easier to calculate the summary element AFTER having
           generated the RGML <path> elements ? -->
    </rgml:picture>
  </xsl:template>

  <!-- prevent echoing of text nodes -->
  <xsl:template match="//svg:tspan" />

  <!-- placeholder for context elements -->
  <xsl:template name="context">
      <rgml:context>
        <rgml:rgb r="1" g="1" b="1" />
        <rgml:style lwd="1" lty=""/>
      </rgml:context>    
  </xsl:template>

  <xsl:template match="//svg:rect">
    <rgml:path id="{generate-id()}" type="stroke">
      <xsl:call-template name="context" />
      <rgml:move>
        <xsl:attribute name="x">
          <xsl:value-of select="@x" />
        </xsl:attribute>
        <xsl:attribute name="y">
          <xsl:value-of select="@y" />
        </xsl:attribute>
      </rgml:move>
      <rgml:line>
        <xsl:attribute name="x">
          <xsl:value-of select="@x" />
        </xsl:attribute>
        <xsl:attribute name="y">
          <xsl:value-of select="@y + @height" />
        </xsl:attribute>
      </rgml:line>
      <rgml:line>
        <xsl:attribute name="x">
          <xsl:value-of select="@x + @width" />
        </xsl:attribute>
        <xsl:attribute name="y">
          <xsl:value-of select="@y + @height" />
        </xsl:attribute>
      </rgml:line>
      <rgml:line>
        <xsl:attribute name="x">
          <xsl:value-of select="@x + @width" />
        </xsl:attribute>
        <xsl:attribute name="y">
          <xsl:value-of select="@y" />
        </xsl:attribute>
      </rgml:line>
      <rgml:line>
        <xsl:attribute name="x">
          <xsl:value-of select="@x" />
        </xsl:attribute>
        <xsl:attribute name="y">
          <xsl:value-of select="@y" />
        </xsl:attribute>
      </rgml:line>
    </rgml:path>
  </xsl:template>

</xsl:stylesheet>
