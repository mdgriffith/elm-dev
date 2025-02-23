module Test.Frontmatter exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Generate.Assets
import Parser
import Test exposing (Test)


runParser string =
    Parser.run Generate.Assets.frontmatterParser string


suite : Test
suite =
    Test.describe "Frontmatter"
        [ parsing
        ]


parsing : Test
parsing =
    Test.only <|
        Test.describe "Parsing"
            [ Test.test "Empty string means no frontmatter" <|
                \_ ->
                    case runParser "" of
                        Err err ->
                            Expect.fail
                                ("Failed to parse empty string?: " ++ Debug.toString err)

                        Ok _ ->
                            Expect.pass
            , Test.test "Parse frontmatter till the first header" <|
                \_ ->
                    case runParser spells of
                        Err err ->
                            Expect.fail
                                ("Failed to parse empty string?: " ++ Debug.toString err)

                        Ok parsed ->
                            let
                                _ =
                                    Debug.log "Parsed" parsed
                            in
                            case parsed.attrs of
                                [ ( "description", _ ) ] ->
                                    Expect.pass

                                _ ->
                                    Expect.fail
                                        ("Failed to extract description field: " ++ Debug.toString parsed)
            , Test.test "Trim frontmatter" <|
                \_ ->
                    let
                        toTrim =
                            """description: General information about spells from the 5th Edition (5e) SRD (System Reference Document).

#"""
                    in
                    Expect.equal
                        (Generate.Assets.trimFrontMatter toTrim)
                        "description: General information about spells from the 5th Edition (5e) SRD (System Reference Document)."
            ]


spells =
    """description: General information about spells from the 5th Edition (5e) SRD (System Reference Document).

# What Is a Spell? 
A spell is a discrete magical effect, a single shaping of the magical energies that suffuse the multiverse into a specific, limited expression. In casting a spell, a character carefully plucks at the invisible strands of raw magic suffusing the world, pins them in place in a particular pattern, sets them vibrating in a specific way, and then releases them to unleash the desired effect--in most cases, all in the span of seconds.

Spells can be versatile tools, weapons, or protective wards. They can deal damage or undo it, impose or remove conditions, drain life energy away, and restore life to the dead.

Uncounted thousands of spells have been created over the course of the multiverse's history, and many of them are long forgotten. Some might yet lie recorded in crumbling spellbooks hidden in ancient ruins or trapped in the minds of dead gods. Or they might someday be reinvented by a character who has amassed enough power and wisdom to do so. 

## Spell Level 
Every spell has a level from 0 to 9. A spell's level is a general indicator of how powerful it is, with the lowly (but still impressive) **_magic missile_** at 1st level and the earth-shaking **_wish_** at 9th. Cantrips--simple but powerful spells that characters can cast almost by rote--are level 0. The higher a spell's level, the higher level a spellcaster must be to use that spell.

Spell level and character level don't correspond directly. Typically, a character has to be at least 17th level, not 9th level, to cast a 9th-level spell. 

## Known and Prepared Spells 
Before a spellcaster can use a spell, he or she must have the spell firmly fixed in mind, or must have access to the spell in a magic item. Members of a few classes, including bards and sorcerers, have a limited list of spells they know that are always fixed in mind. The same thing is true of many magic-using monsters. Other spellcasters, such as clerics and wizards, undergo a process of preparing spells. This process varies for different classes, as detailed in their descriptions.

In every case, the number of spells a caster can have fixed in mind at any given time depends on the character's level. 

## Spell Slots 
Regardless of how many spells a caster knows or prepares, he or she can cast only a limited number of spells before resting. Manipulating the fabric of magic and channeling its energy into even a simple spell is physically and mentally taxing, and higher- level spells are even more so. Thus, each spellcasting class's description (except that of the warlock) includes a table showing how many spell slots of each spell level a character can use at each character level. For example, the 3rd-level wizard Umara has four 1st-level spell slots and two 2nd-level slots.

When a character casts a spell, he or she expends a slot of that spell's level or higher, effectively "filling" a slot with the spell. You can think of a spell slot as a groove of a certain size--small for a 1st-level slot, larger for a spell of higher level. A 1st-level spell fits into a slot of any size, but a 9th-level spell fits only in a 9th-level slot. So when Umara casts **_magic missile_**, a 1st-level spell, she spends one of her four 1st-level slots and has three remaining.

Finishing a long rest restores any expended spell slots.

Some characters and monsters have special abilities that let them cast spells without using spell slots. For example, a monk who follows the Way of the Four Elements, a warlock who chooses certain eldritch invocations, and a pit fiend from the Nine Hells can all cast spells in such a way. 

## Casting a Spell at a Higher Level 
When a spellcaster casts a spell using a slot that is of a higher level than the spell, the spell assumes the higher level for that casting. For instance, if Umara casts **_magic missile_** using one of her 2nd-level slots, that **_magic missile_** is 2nd level. Effectively, the spell expands to fill the slot it is put into.

Some spells, such as **_magic missile_** and **_cure wounds_**, have more powerful effects when cast at a higher level, as detailed in a spell's description. 

> ### Casting in Armor 
> Because of the mental focus and precise gestures required for spellcasting, you must be proficient with the armor you are wearing to cast a spell. You are otherwise too distracted and physically hampered by your armor for spellcasting.

## Cantrips 
A cantrip is a spell that can be cast at will, without using a spell slot and without being prepared in advance. Repeated practice has fixed the spell in the caster's mind and infused the caster with the magic needed to produce the effect over and over. A cantrip's spell level is 0. 

## Rituals 
Certain spells have a special tag: ritual. Such a spell can be cast following the normal rules for spellcasting, or the spell can be cast as a ritual. The ritual version of a spell takes 10 minutes longer to cast than normal. It also doesn't expend a spell slot, which means the ritual version of a spell can't be cast at a higher level.

To cast a spell as a ritual, a spellcaster must have a feature that grants the ability to do so. The cleric and the druid, for example, have such a feature. The caster must also have the spell prepared or on his or her list of spells known, unless the character's ritual feature specifies otherwise, as the wizard's does.
"""
