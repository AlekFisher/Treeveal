# Adelphi App Theme Guide

This file captures the visual direction used for the SIMCOM app so the same look and feel can be applied across future Adelphi internal/client-facing apps.

Reference checked against:
- `https://adelphiresearch.com/`
- `www/AdelphiLogo_Drive.png`
- SIMCOM shell styles in `www/simcom-ux.css`

Checked on: 2026-03-27

## Goal

Future apps should feel like part of one Adelphi product family:
- professional
- clear
- modern but restrained
- brand-aligned rather than generic Bootstrap
- consistent enough that users recognize a shared suite

## Brand Direction

The Adelphi visual language in the current implementation is built around:
- teal as the primary brand color
- dark slate for text and structural contrast
- white and very light aqua backgrounds for cleanliness
- yellow/gold as a controlled accent
- minimal use of other bright colors unless data visualization requires them

The tone should feel:
- confident
- strategic
- healthcare-professional
- polished, not flashy

## Core Palette

Recommended base palette:

```text
Primary teal:        #00A6AD
Deep teal:           #007F8F
Dark teal/navy:      #00566F
Slate text:          #24313B
Secondary text:      #51626F
Page background:     #F3FBFA
Soft surface:        #F9FCFB
Card surface:        #FFFFFF
Accent yellow:       #F2B82D
Accent yellow hover: #F5C95B
Border teal tint:    rgba(0, 89, 110, 0.08 to 0.16)
```

Usage rules:
- Use teal for active states, key accents, badges, section labels, and primary navigation emphasis.
- Use dark teal for deeper surfaces such as hero gradients, info panels, and strong CTA backgrounds.
- Use yellow only for highlights and primary CTA emphasis. Do not flood the interface with yellow.
- Keep body text in slate/dark gray, not pure black.

## Typography

Use:
- UI/body: `"Aptos", "Segoe UI Variable Text", "Segoe UI", sans-serif`
- Headlines: `Georgia, "Times New Roman", serif`

Typography rules:
- Headlines should feel editorial and deliberate, not like default dashboard labels.
- Use serif headings for hero titles and major section titles.
- Use sans-serif for body copy, labels, filters, and navigation.
- Keep copy concise and functional. Avoid marketing-heavy filler.

## Layout Principles

All Adelphi-themed apps should follow these shell rules:
- Soft tinted page background, never flat white edge-to-edge.
- A distinct branded header area near the top.
- Rounded surfaces with subtle shadows.
- Strong spacing rhythm with generous breathing room.
- Navigation should feel integrated into the shell, not dropped in by default Bootstrap.
- Filters should sit in a deliberate toolbar or utility area, not float awkwardly inside content.

Recommended shell pattern:
1. Branded header with app/product identity and Adelphi logo.
2. Utility toolbar for global selectors or context filters.
3. Styled navigation bar or tab bar.
4. Main content area with card/surface sections.

## Component Patterns

### Header

Use a branded header block with:
- app logo or product mark on the left
- app title and one-line purpose statement
- Adelphi logo on the right
- soft gradient or light tinted surface

### Navigation

Use:
- rounded tabs/pills
- teal active state
- subtle hover state
- slightly elevated container

Avoid:
- plain default Bootstrap tabs
- bright blue defaults
- crowded nav text

### Hero Section

For landing pages:
- use a deep teal gradient
- use serif headline
- keep supporting copy practical and short
- include one primary CTA and one secondary CTA
- include a compact context panel with real study/app information where possible

### Cards / Surfaces

Cards should use:
- white or very pale aqua backgrounds
- rounded corners around `18px` to `30px`
- soft shadow, not heavy drop shadow
- thin teal-tinted borders

### Buttons

Primary button:
- yellow fill
- dark text
- rounded pill shape

Secondary button:
- transparent or lightly tinted
- white or slate text depending on surface
- visible border

### Labels / Eyebrows

Use uppercase micro-labels for section intros:
- teal
- small font size
- wide letter spacing

## Content Tone

Write UI copy as if it belongs to an established Adelphi application:
- explain what the user can do
- explain where to go next
- describe metrics or outputs clearly
- keep language direct and credible

Do not include:
- references to the app being redesigned or refreshed
- meta commentary about the UI itself
- generic startup-marketing phrases that could belong to any SaaS homepage

Preferred copy style:
- "Review top-performing bundles across key metrics."
- "Use the global cut selector to validate recommendations by audience."
- "Open Methodology to confirm the active reach definition."

## Data-App Specific Guidance

When applying this theme to analytical apps:
- keep decorative color use in the shell, not inside the charts unless it helps interpretation
- chart colors can remain study-driven, but surrounding chrome should stay Adelphi-themed
- use the same shell/header/navbar/filter styling even if the app content differs
- ensure users can recognize the suite even when each app has different metrics or workflows

## Reusable CSS Structure

For future apps, start with CSS variables like:

```css
:root {
  --adelphi-primary: #00A6AD;
  --adelphi-primary-deep: #007F8F;
  --adelphi-primary-dark: #00566F;
  --adelphi-text: #24313B;
  --adelphi-text-muted: #51626F;
  --adelphi-bg: #F3FBFA;
  --adelphi-surface: #FFFFFF;
  --adelphi-surface-soft: #F9FCFB;
  --adelphi-accent: #F2B82D;
  --adelphi-accent-hover: #F5C95B;
  --adelphi-border: rgba(0, 89, 110, 0.10);
  --adelphi-shadow: 0 18px 44px rgba(16, 52, 68, 0.08);
  --adelphi-radius-lg: 28px;
  --adelphi-radius-md: 18px;
}
```

Then structure styles around:
- shell
- header
- toolbar
- nav
- hero
- cards
- buttons
- responsive rules

## Implementation Checklist

When styling a new Adelphi app, check all of the following:
- Page background uses Adelphi light tint rather than flat white.
- Header includes Adelphi branding and app identity.
- Navigation active state uses teal.
- Primary CTA uses Adelphi yellow.
- Headlines use the serif style for major sections.
- Utility filters sit in a dedicated toolbar area.
- Card borders/shadows are subtle and consistent.
- Copy talks about the workflow and outputs, not the interface changes.
- Mobile widths still render cleanly.
- Charts and tables remain readable and are not overwhelmed by decorative styling.

## What To Avoid

Avoid these patterns in future apps:
- default Bootstrap blue buttons and tabs
- pure-black text on stark white everywhere
- purple gradients or unrelated brand colors
- overly dark enterprise dashboards
- heavy glassmorphism or overly animated UI
- inconsistent fonts between apps
- verbose marketing hero copy unrelated to the actual workflow

## Recommended Reuse Path

For future work, the cleanest approach is:
1. Copy the design tokens and shell structure from `www/simcom-ux.css`.
2. Reuse the same header/navbar/toolbar/card conventions.
3. Change only the product-specific content and workflow sections.
4. Keep palette, typography, spacing, and CTA rules consistent.

If this becomes a recurring pattern across multiple apps, promote the shared shell CSS into a reusable Adelphi app stylesheet rather than duplicating theme rules per app.
