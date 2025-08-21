<TeXmacs|2.1.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|llm|1.0>

    <\src-purpose>
      Markup for LLM sessions.
    </src-purpose>

    <\src-copyright|2021>
      Joris van der Hoeven

      \ \ \ \ 2024 by Darcy Shen
    </src-copyright>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|session>

  <assign|boxed|<macro|x|<ornament|<arg|x>>>>

  <assign|llm-prompt-color|dark green>

  <assign|llm-input-color|black>

  <\active*>
    <\src-comment>
      Use verbatim output
    </src-comment>
  </active*>

  <assign|llm-output|<\macro|body>
    <\with|font-family|CMU>
      <\generic-output>
        <text|<arg|body>>
      </generic-output>
    </with>
  </macro>>

  <assign|llm-errput|<\macro|body>
    <\with|mode|text|language|verbatim|font-family|CMU>
      <\generic-errput>
        <arg|body>
      </generic-errput>
    </with>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>