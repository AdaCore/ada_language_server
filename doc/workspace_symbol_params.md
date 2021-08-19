# WorkspaceSymbolParams

## Short introduction

This feature allows the LSP server to provide results for more flexible
search conditions like:
  -  A Start_Word_Text works like a Full_Text but tested word should match
     patters from the first letter. This is the default choice.

  -  A Full_Text match searches the pattern exactly in the contents.

  -  A regexp parses the pattern as a regular expression.

  -  A fuzzy match will search for some contents that contains all the
     characters of the pattern, in the same order, but possibly with
     other characters in-between. The number of characters in-between is not
     limited, so this mode really only makes sense when matching short text
     (and not, for instance, in text editors).

  -  Approximate allows one or two errors to appear in the match (character
     insertion, deletion or substitution). This is mostly suitable when
     matching in long texts. The implementation of this algorithm is
     optimized so that characters are matched only once, but the total length
     of the pattern is limited to 64 characters. The exact number of errors
     depends on the length of the pattern:
         patterns of length <= 4  => no error allowed
         patterns of length <= 10 => one error allowed
         long patterns            => up to two errors



## Change description

We extend the `WorkspaceSymbolParams` by adding extra fields:
  - case_sensitive to take letters' case into account
  - whole_word to match the whole word instead of a part of it when True
  - negate to invert matching
  - kind is a search kind described above

```typescript

export type Search_Kind = 'Full_Text' | 'Regexp' | 'Fuzzy' | 'Approximate' | 'Start_Word_Text';

export namespace Search_Kind {
	export const Full_Text = 1;
	export const Regexp = 2;
	export const Fuzzy = 3;
	export const Approximate = 4;
	export const Start_Word_Text = 5;
}

interface WorkspaceSymbolParams {
	query: string;
	case_sensitive?: boolean;
	whole_word?: boolean;
	negate?: boolean;
	kind?: Search_Kind;
}
```
