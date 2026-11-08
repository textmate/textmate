# Task Completion Checklist

When a coding task is completed, ensure the following:

1. **Build check:** Run `make debug` to verify the project builds without errors.
2. **Test affected frameworks:** Run `cd build-debug && ctest --output-on-failure -R <framework>` for any framework touched during the task.
3. **Code style:** Verify code follows project conventions (3-space hard tabs, `type* var` pointer style, `_t` type suffix, appropriate namespaces).
4. **No new warnings:** The build should not introduce new compiler warnings.
5. **Commit message format:** Summary < 70 chars, blank line, then reasoning about the change.
