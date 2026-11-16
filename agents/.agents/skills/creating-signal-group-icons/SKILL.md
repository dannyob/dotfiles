---
name: creating-signal-group-icons
description: Use when asked for a group picture, profile icon or avatar for a Signal group or another chat app's round avatar, showing real people (the user, friends, family, colleagues) as cartoons or illustrations drawn from their photos, or to restyle one made earlier.
---

# Creating Signal group icons

A Signal group icon is a square image that Signal crops to a circle and
mostly shows about 48px across. These are cartoons of real people, made by
an image model from reference photos through OpenRouter's Image API, then
chosen by the user from circle-cropped sheets. The two scripts are in
`scripts/` beside this file; both print their usage when run without
arguments. The commands below call that directory `$K`:

    K=~/.agents/skills/creating-signal-group-icons/scripts

## Before generating

Find out, from the request or by asking:

- **Photos of each person, and who is who.** You can't recognise faces.
  Use file names, folders, tags or the user's word. Prefer two recent photos
  per person that agree with each other. If a person's photos show
  different looks (beard or none, hat or none), ask which is current.
- **How each person wants to come across** ("friendly", "butch", "laughing
  and a bit fierce"). Their words go into the prompt, and change the
  result more than anything else.
- **The idea and the look**: what the group is doing, and any house style
  or logo to use.
- **A yes to sending the photos to an outside service**, for this job. An
  earlier job's yes doesn't count.

## Steps

1. Make a working directory in the scratchpad and a durable one for
   candidates, `~/tmp/NAME-icon/`. Keep images out of synced folders and
   repositories.
   Give every file a name that differs by more than letter case: macOS
   treats `a.txt` and `A.txt` as one file.
2. Shrink each photo to 1024px on its long side into `refs/` in the
   working directory (`magick IN -auto-orient -resize '1024x1024>' OUT`),
   then look at them.
3. Write the prompt to `prompt.txt`, following "The prompt" below.
4. Generate three drafts at once. Drafts are named by letter, continuing
   the alphabet across rounds (a, b, c, then d, e, f), so that "G" always
   means one image. Run this as one Bash call with a five-minute timeout,
   from the working directory, with the references in the order the prompt
   numbers them:

       export OPENROUTER_API_KEY=$(pass show PATH/TO/OPENROUTER-KEY | head -1)
       for d in a b c; do
         uv run --script $K/genimg openai/gpt-image-2.6-sunburst prompt.txt out/$d \
           refs/REF1 refs/REF2 ... -- quality=high background=opaque \
           'provider={"data_collection":"deny"}' > out-$d.log 2>&1 &
       done; wait; cat out-?.log

   The key comes from wherever the user keeps secrets; never print it. A
   draft takes about a minute and costs about 10 cents; each log ends
   with its `cost:`. If a call times out, check openrouter.ai/activity
   before running it again; it may have been billed.
5. Build the sheet and look at it yourself before the user does:

       uv run --script $K/iconsheet sheet-1.png out/a.png out/b.png out/c.png

   Check each draft: the right number of people, each with their own hair,
   glasses and beard; no text; faces inside the circle; still readable in
   the 96px copy. Fix any transparency warning (see Mistakes).
6. Copy the drafts worth showing to the durable directory, build a sheet
   of those there, and show it (SendUserFile if you have it, otherwise
   `open`). Say which you would pick and why, what the round cost, and ask
   who doesn't look like themselves. Likeness is for the people in the
   picture to judge.
7. For a change of style, restyle the draft the user picked (below) instead of
   starting again.

## The prompt

`prompt-first-round.txt` is adapted from one that worked (the people in
it are invented). Its parts, in order:

1. What it is for: a group chat picture, cropped to a circle, shown small.
2. One paragraph per person, called Person A, B, C and tied to the
   reference images by number ("reference photos 3 and 4"). No names.
   Describe hair, glasses and beard as the photos show them, and clothes
   too unless the scene calls for others. Refer to each person as the user
   does. Then the expression: the person's own words, and what that looks
   like on a face. Add what to avoid, from what they asked for ("no hat";
   for someone who wants to look butch, "do not feminise them").
3. Composition: where each person sits, and what the lens does.
4. Style, and that it must read at 48 pixels. Name the style's properties
   (outline weight, flat colour, how much shading), not an artist.
5. "Keep all faces inside the circle inscribed in the square. Fill the
   whole square with the background: fully opaque. No text, no border, no
   drawn circle."

Send each photo as its own reference image, without captions drawn on it.

## Restyling a draft

Pass the chosen draft as reference image 1, then any image that shows the
look wanted (a logo, a screenshot of an effect), then one photo per person.
`prompt-restyle.txt` is adapted from one that worked: it says what to keep from image 1
(the people, their positions and expressions), names each new property of
the style, and gives a logo a place and a size ("as a round embroidered
patch on the shoulder of the jacket"). Give exact colours as hex values.
Afterwards crop in on the logo and check that it is drawn correctly
(the numbers are the crop's size and its top-left corner):

    magick out/g.png -crop 400x400+600+220 +repage logo-check.png

If the look has a written guide (a logo's README, a site's stylesheet),
take the colours from it. An effect from a program can be shown with one
frame of its output.

## Models

`openai/gpt-image-2.6-sunburst` kept three real faces distinct and bent
the picture as asked. Google's Gemini image models drew generic faces from
the same photos. Models change: `curl -s https://openrouter.ai/api/v1/images/models`
lists the current ones, and
`.../images/models/MODEL/endpoints` gives each one's price and how many
reference images it takes. Try a new model with one draft before spending
a round on it. The API is described at
https://openrouter.ai/docs/features/multimodal/image-generation.md.

## Mistakes

| What happens | Why | Fix |
|---|---|---|
| Corners come back transparent or faded | The model sometimes masks the picture to the circle itself | Always pass `background=opaque` and ask for a filled square. To rescue a draft: read the background colour with `magick FILE -format '%[pixel:p{X,Y}]' info:`, then `iconsheet --flatten '#RRGGBB' FILE`, which changes the file in place |
| A person looks generic, or younger | One photo, an old photo, or a weak model | Two recent photos; the model above |
| A person drawn twice, or two people merged | Photos of one person that disagree | One look per person |
| Words or labels in the picture | Captions on the reference images | Plain photos; "No text" in the prompt |
| A command runs for minutes and a file grows without limit | `cat x.txt y.txt > Y.txt` on macOS reads the file it is writing | Distinct names; check sizes with `ls -la` |
| Heads cut by the circle | Models ignore requests for a margin | Usually fine for a crowded picture; otherwise restyle with "pull the heads toward the centre" |
