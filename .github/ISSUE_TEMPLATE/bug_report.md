name: Bug Report
about: Report a bug in dataviewR
title: "[BUG] "
labels: bug
assignees: ''

body:
  - type: markdown
    attributes:
      value: "## Describe the bug clearly below:"
  - type: textarea
    id: description
    attributes:
      label: "Bug Description"
      description: "Please describe the problem in detail."
    validations:
      required: true
  - type: textarea
    id: reproducible
    attributes:
      label: "Reproducible Steps"
      description: "Steps to reproduce this bug."
      placeholder: "1. Run this function...\n2. Click this button..."
