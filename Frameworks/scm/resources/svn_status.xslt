<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>

  <xsl:template match="/">
    <xsl:apply-templates select="/status/target"/>
  </xsl:template>

  <xsl:template match="/status/target/entry">
    <xsl:value-of select="@path" />
  </xsl:template>

  <!-- Write out parseable format: FILE_STATUS    FILE_PROPS_STATUS    FILE_PATH -->
  <xsl:template match="/status/target/entry">
     <xsl:value-of select="wc-status/@item"/>
     <xsl:value-of select="'    '"/>
     <xsl:value-of select="wc-status/@props"/>
     <xsl:value-of select="'    '"/>
     <xsl:value-of select="@path"/>
  </xsl:template>
</xsl:stylesheet>
