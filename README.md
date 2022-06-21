# Whitespace issue in syntax rewriter

The unit test checks if the whitespace `"    "` is preserved.
The actual code fix instead, emits an extra `"        "`.

The result from the actual execution, is that no whitespace is preserved at all.
Even though the new document's syntax root will contain it.
